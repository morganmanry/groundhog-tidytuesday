# Shiny Web Application - TidyTuesday Groundhogs Day
# MM - 08/16/2024

# import packages
library(shiny)
library(leaflet)
library(tidyverse)
library(RColorBrewer)
library(bslib)
library(bsicons)
library(DT) # R interface to the JavaScript library DataTables

# import dataset from tidytuesday
tuesdata <- tidytuesdayR::tt_load(2024, week = 5)
groundhogs <- tuesdata$groundhogs
predictions <- tuesdata$predictions

# merge, keep variables of interest
ground_clean <- predictions |>
  left_join(groundhogs) |>
  filter(!is.na(shadow)) |>
  select(id, year, shadow, region, country, latitude, longitude, predictions_count) |>
  group_by(region) |>
  mutate(
    av_shadow = mean(shadow, na.rm = TRUE)
  ) |>
  ungroup()

# 'Cards' for dashboard / front page
card1 <- card(
  card_header("Objectives"),
  class = "bg-primary",
  "How have groundhog shadow spotting changed over time? What areas have higher rates of shadow spotting?")

card2 <- card(
  card_header("Dataset"),
  class = "bg-primary",
  "From TidyTuesday via github; published 2024-01-30")

card3 <- card(
  card_header("Motivation"),
  class = "bg-primary",
  "Made for Data Visualizations class for SISBID 2024")

# define organization or 'body'
body <- page_fillable(
  layout_columns(
    col_widths = c(4, 4, 4),
    row_heights = "200px",
    card1, card2, card3
  )
)

# define theme for website
theme <- bs_theme(
  # Controls the default grayscale palette
  bg = "#202123", fg = "#B8BCC2",
  # Controls the accent (e.g., hyperlink, button, etc) colors
  primary = "#196f3d", secondary = "#FF5733",
  base_font = c("Helvetica", "sans-serif"),
  code_font = c("Courier", "monospace"),
  heading_font = "'Helvetica Neue', Helvetica, sans-serif",
  # Can also add lower-level customization
  "input-border-color" = "#DAF7A6"
)

# define user interface
ui <- bslib::page_navbar(
  title = "Groundhogs Day",
  theme = theme,
  #sidebar = side,
  nav_panel(title = "Dashboard", body,
            icon = bs_icon("bar-chart", ally = "deco")),
  nav_panel(title = "Shadows",
            icon = bs_icon("shadows", ally = "deco"),
            h2("How many shadows spotted over time?"),
            selectInput("region", "Enter a region:",
                                    choices = unique(ground_clean$region),
                                    multiple = TRUE,
                                    selected = "Ontario"),
            plotOutput("scatter", height = "1600px"),
            DTOutput("mytable")),
  nav_panel(title = "Map",
            icon = bs_icon("globe-americas", ally = "deco"),
            h2("Distribution of Groundhog Entries across North America by shadow-success rate"),
            leafletOutput("map_na"))
  )

server <- function(input, output, session) {
  output$scatter = renderPlot({
    ground_clean %>%
      filter(region %in% input$region) %>%
      ggplot(aes(x = year, fill = region)) +
        geom_histogram() +
        scale_fill_brewer(palette = "Dark2")
  })

  output$mytable <- DT::renderDT({
    ground_clean %>%
      select(-id) %>%
      group_by(region, year) %>%
      arrange(desc(av_shadow))
  })

  output$map_na = renderLeaflet({
    pal <- colorNumeric("Dark2", domain = range(ground_clean$av_shadow))
    ground_clean |>
      filter(!is.na(latitude),
             !is.na(longitude)) |>
      leaflet() |>
      addTiles() |>
      addCircleMarkers(
        opacity = 0.5,
        stroke = FALSE,
        color = ~ pal(ground_clean$av_shadow),
        #color = ~ colorBin("Dark2", ground_clean$av_shadow, 6),
        label = ~ av_shadow,
        radius = ~ (av_shadow*9),
        lat = ~latitude, 
        lng = ~longitude) |> 
      addLegend(
       position = c("bottomright"),
       pal = pal,
       values = range(ground_clean$av_shadow),
       title = "% Shadow Seen"
      )
    # got rid of this but this would use Google ggmap()
    #get_map(location = 'US', zoom = 3) %>%
    #ggmap() +
     # geom_point(data = ground_clean,
      #           aes(x = longitude,
       #              y = latitude,
        #             size = av_shadow,
         #            fill = av_shadow,
          #           color = av_shadow,
           #          alpha = 0.5)
            #     ) +
     # ggtitle("Groundhog Locations in North America")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
