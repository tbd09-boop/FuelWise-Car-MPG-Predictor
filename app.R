library(shiny)

# Fuel efficiency prediction model
model <- lm(mpg ~ wt + hp + am, data = mtcars)

# User interface
ui <- fluidPage(
    
    titlePanel("FuelWise - Car Fuel Efficiency Predictor"),
    
    p("Estimate a car's fuel efficiency using its weight,
    horsepower and transmission type."),
    
    sidebarLayout(
        sidebarPanel(
            
            sliderInput("weight", "Vehicle Weight (1000 lbs):",
                        min = 1.5, max = 5.5, value = 3, step = 0.1),
            
            sliderInput("hp", "Horsepower:",
                        min = 50, max = 335, value = 120),
            
            selectInput("am", "Transmission:",
                        choices = c("Automatic" = 0, "Manual" = 1)),
            
            checkboxInput("showPlot", "Show comparison chart", TRUE),
            
            hr(),
            
            h4("How to Use FuelWise"),
            
            p("1. Adjust the vehicle weight slider."),
            p("2. Select the horsepower."),
            p("3. Choose automatic or manual transmission."),
            p("4. View your predicted fuel efficiency."),
            
            p("The prediction updates automatically."),
            
            hr(),
            
            p("Note: Predictions use the historical mtcars
        dataset and are intended for educational purposes.")
        ),
        
        mainPanel(
            
            h3("Predicted Fuel Efficiency"),
            
            h2(textOutput("prediction")),
            
            hr(),
            
            conditionalPanel(
                condition = "input.showPlot == true",
                plotOutput("fuelPlot")
            ),
            
            h4("About the Prediction"),
            
            p("FuelWise uses linear regression to estimate
        miles per gallon (MPG) based on vehicle weight,
        horsepower and transmission.")
        )
    )
)

# Server calculations
server <- function(input, output) {
    
    predicted_mpg <- reactive({
        
        new_car <- data.frame(
            wt = input$weight,
            hp = input$hp,
            am = as.numeric(input$am)
        )
        
        predict(model, newdata = new_car)
    })
    
    output$prediction <- renderText({
        paste(round(predicted_mpg(), 1), "Miles Per Gallon")
    })
    
    output$fuelPlot <- renderPlot({
        
        plot(mtcars$wt, mtcars$mpg,
             main = "Vehicle Weight vs Fuel Efficiency",
             xlab = "Weight (1000 lbs)",
             ylab = "Miles Per Gallon",
             col = "steelblue",
             pch = 19)
        
        points(input$weight, predicted_mpg(),
               col = "red", pch = 19, cex = 2)
        
        legend("topright",
               legend = c("Historical Cars", "Your Prediction"),
               col = c("steelblue", "red"),
               pch = 19)
    })
}

# Launch application
shinyApp(ui = ui, server = server)