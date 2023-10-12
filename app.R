# Load necessary packages

  
library(tidymodels)
library(shiny)
library(lubridate)
library(ranger)
library(readr)

#rsconnect::setAccountInfo(name='perfilesderiesgo',
                         # token='3532D8313FD973C7F6C3DD342943285E',
                        #  secret='7rLz7usfFbpZFubLzLBgoOowb9KpL46cWBzWqp8v')# Set working directory and load data and model

#setwd("~/Dropbox/Back up todo/Sernapesca Project/Models and Data/ML/SernapescaApp")
rf_fit <- read_rds("rf_fit_model.rds")
illegal = read.csv("illegal.csv")
species = read.csv("species.csv")

# UI component of the Shiny app
ui <- fluidPage(
  
  titlePanel("App Predicción Infracciones"),
 tags$img(src='logos.png', height=200, width=600),
  sidebarLayout(
    sidebarPanel(
      dateInput("input_date", "Fecha a Realizar el Cometido:", Sys.Date(), format = "dd/mm/yyyy"),
      selectInput("input_hora", "Hora a Realizar el Cometido:", choices = sort(unique(illegal$hora))),
      selectInput("region", "Region:", choices = unique(illegal$region)),
      selectInput("oficina", "Oficina:", choices = ""),
      selectInput("especie", "Especie:", choices = ""),
      selectInput("agente", "Agente:", choices = ""),
      sliderInput("tiempo", "Duración del Cometido (horas):", min = 1, max = 14, value = 1),
      sliderInput("n_fiscalizadores_por_cometido", "Número Fiscalizadores en el Cometido:", min = 1, max = 7, value = 1),
      actionButton("predict", "Predecir")
    ),
    
    mainPanel(
      uiOutput("prediction"),
      uiOutput("species_av"),
      uiOutput("prediction_category")
    )
  )
)


# Server component of the Shiny app
server <- function(input, output, session) {
  
  # Update office choices based on selected region
  shiny :: observe({
    filtered_offices <- illegal %>% 
      filter(region == input$region) %>% 
      distinct(oficina) %>% 
      pull(oficina)
    updateSelectInput(session, "oficina", choices = filtered_offices)
  })
  
  
  # Update species choices based on selected office
  shiny :: observe({
    filtered_species <- illegal %>% 
      filter(oficina == input$oficina) %>% 
      distinct(especie) %>% 
      pull(especie)
    updateSelectInput(session, "especie", choices = filtered_species)
  })
  
  # Update medida choices based on selected species
  shiny :: observe({
    filtered_agente <- illegal %>% 
      filter(especie == input$especie) %>% 
      distinct(agente) %>% 
      pull(agente)
    updateSelectInput(session, "agente", choices = filtered_agente)
  })
  
  
  
  # Prediction logic
  observeEvent(input$predict, {

    showModal(modalDialog(
      div(style = "text-align: center",
          em(strong("Realizando Predicción ..."))),
      footer = NULL,
      size = "s",
      fade = TRUE
    ))
    
    # Calculate week number from input date
    semana <- week(input$input_date)
    dia    <- wday(input$input_date)
    hora   <- input$input_hora
    
    new_data <- tibble(
      tiempo = input$tiempo,
      n_fiscalizadores_por_cometido = input$n_fiscalizadores_por_cometido,
      medida = input$medida,
      agente = input$agente,
      especie = input$especie,
      region = input$region,
      oficina = input$oficina,
      semana = semana,
      dia = dia,
      hora = hora
    )
    
    prediction <- predict(rf_fit, new_data, type="prob")
    prediction_percentage <- (prediction[1,2]) * 100
    
    aa = input$especie
    avg_prob <- subset(species, species$.pred_No == aa) 
    avg_prob <- avg_prob$.pred_Si 
    threshold <- avg_prob / 2 # Set this to your desired threshold
    
    if (prediction_percentage > avg_prob + threshold) {
      category <- "good"
    } else if (prediction_percentage < avg_prob - threshold) {
      category <- "poor"
    } else {
      category <- "regular"
    }
    
    # Inside your server function, after categorizing the prediction
    output$prediction_category <- renderUI({
      if (category == "good") {
        color <- "green"
        text <- "Cometido Sobre el Promedio de la Pesquería"
        font_size <- "20px"
        font_family <- "'Arial', sans-serif"
        font_weight <- "bold"
      } else if (category == "regular") {
        color <- "black"
        text <- "Cometido en el Promedio de la Pesquería"
        font_size <- "20px"
        font_family <- "'Arial', sans-serif"
        font_weight <- "bold"
      } else {
        color <- "red"
        text <- "Cometido Bajo el Promedio de la Pesquería"
        font_size <- "20px"
        font_family <- "'Arial', sans-serif"
        font_weight <- "bold"
      }
      
      tags$div(style = sprintf("color: %s;font-size: %s", color, font_size), text)
    })
    
    # Display prediction
    output$prediction <- renderUI({
      tagList(
        h3("Probabilidad de Encontrar una Infracción en el Cometido:",  style="font-size: 30px;"),
        tags$div(
          class = "progress",
          tags$div(
            class = "progress-bar progress-bar-striped active",
            role = "progressbar",
            style = sprintf("width: %s%%;", prediction_percentage),
            sprintf("%s%%", round(prediction_percentage))
          )
        )
      )
    })
    
    output$species_av <- renderUI({
      tagList(
        h3("Promedio", aa, style="color:black;", style="font-size: 18px;"),
        tags$div(
          class = "progress",
          tags$div(
            class = "progress-bar progress-bar-striped active",
            role = "progressbar",
            style = sprintf("width: %s%%;", avg_prob),
            sprintf("%s%%", round(avg_prob))
          )
        )
      )
    }) 
    removeModal()
  })
}

# Run the Shiny app
shinyApp(ui, server)