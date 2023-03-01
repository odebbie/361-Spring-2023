#
# This is a Shiny web application that deploys at https://odebbie.shinyapps.io/hw03app2/


library(shiny)
library(leaflet)
library(tidyverse)


d = readRDS("data/EVstations.rds")
colnames(d) = tolower(colnames(d))

top5networks = c('Tesla',
                 'Electrify America', 
                 'ChargePoint Network', 
                 'eVgo Network', 
                 'Non-Networked')

d = d %>%
  filter(is.na(open.date) == F) %>% 
  select(-matches('cng|lng|lpg|hydrogen|e85|bd[.]blends|french|^ng[.]|plus4|level1')) %>%
  filter(fuel.type.code=='ELEC') %>%
  rename(lev2=ev.level2.evse.num, 
         lev3=ev.dc.fast.count, 
         network = ev.network, 
         lat = latitude, 
         lon = longitude) %>%
  mutate(status = case_when(status.code=='E' ~ 'avail', 
                            status.code=='P' ~ 'planned', 
                            status.code=='T' ~ 'temp.unavail', 
                            TRUE ~ ''), 
         lev2 = ifelse(is.na(lev2), 0, lev2), 
         lev3 = ifelse(is.na(lev3), 0, lev3)) %>%
  mutate(network = ifelse(network=='Tesla Destination', 'Tesla', network), 
         network = gsub('Ã©', 'E', network), 
         network = gsub('Ã‰', 'E', network), 
         network = ifelse(network %in% top5networks, network, 'Other'), 
         network = factor(network, levels = c(top5networks, 'Other'))) %>%
  filter(is.na(network) == F)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Charging Stations"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId='state',  #select a state
                      label = 'State/Province', 
                      choices = sort(unique(d$state)), 
                      selected = 'CT', #default is Connecticut
                      multiple = F),
          #Choose dates
          sliderInput("dates",
                      "Open Date",
                      as.Date("2015-01-01"), Sys.Date(), #min and maximum
                      value = c(as.Date("2015-01-01"), Sys.Date()),
                      step = 1),
          #choose network
          checkboxGroupInput("networkchoice",
                        "Network",
                        choiceNames = sort(unique(d$network)),
                        choiceValues = sort(unique(d$network)),
          #all selected by default
                        selected = sort(unique(d$network))),
          #choose level
          radioButtons("level",
                       "Charging Station Level",
                       choices = list("Level 2" = "lev2",
                         "Level 3" = "lev3"),
                       selected = "lev3")
          
        ),

        # Show a plot of the EV
        mainPanel(
           leafletOutput("map")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


    output$map <- renderLeaflet({
      #state input
      d <- d[d$state==input$state, ]
      #date input
      d <- d |>
        filter(between(open.date, min(input$dates), max(input$dates)))
      #network input
      d <- d |>
        filter(network %in% input$networkchoice)
      
      #level 2 or level 3
      d <- d |> #user input
        #so that R doesnt do this multiple times once we get to if else
        mutate(label = paste0("EV Network: "      , network   , "<br/>",
                              "Station Name: "   , station.name, "<br/>", 
                              "Street Address: " , street.address   , "<br/>", 
                              "City: " , city   , "<br/>", 
                              "State: " , state   , "<br/>",
                              "Zip Code: " , zip   , "<br/>",
                              "Number of Charging Stations: ", match(input$level,names(d))))
        

      if(input$level == "lev2"){
        #make a label for the graph
        d <- d |>
          filter(lev2 != 0)# |>
          #select(-lev3) |>
         # mutate(label = paste0(label, "Number of Level 2 Charging Stations: ", input$level)) #change based on level
                                    }
      else {
        d <- d |>
          filter(lev3 != 0) #|>
          #select(-lev2) |>
          #mutate(label = paste0(label, "Number of Level 3 Charging Stations: ", input$level)) #change based on level
      }
      
      #set view to the state selected
      latitude <- mean(d$lat)
      longitude <- mean(d$lon)
      
      d |>
        select(network, station.name, street.address, city, state, zip, lat, lon, match(input$level,names(d)), label) |>
        #begin leaflet
        leaflet(height = 480, width = 800) |>
        addProviderTiles("CartoDB.Positron") |>
        #addTiles() |>
        addCircleMarkers(lng = ~lon,         
                         lat = ~lat,          
                         radius = 3,          
                         color = '#DEB173', 
                         stroke = FALSE,      
                         fillOpacity = 0.5,
                         popup = ~label,
                         label =~ label |> lapply(HTML)) |>
        setView(lng=longitude, lat=latitude, zoom=7)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

#levels not working