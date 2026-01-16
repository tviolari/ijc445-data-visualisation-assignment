install.packages("tidyverse")
install.packages("readODS")
install.packages("patchwork")
install.packages("viridis")
library(tidyverse)
library(readODS)
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)
library(patchwork)
library(tibble)
library(viridis)
library(tidyr)
install.packages("devtools")
devtools::install_github("ricardo-bion/ggradar",
                         dependencies = TRUE, force=TRUE)
library(scales)
library(ggradar)


# How has rail service punctuality in cancellations in the UK changed over the years and operators?

# load the data first

train_canc <- read_ods("trains-cancellations.ods")

# Selecting specific operators

my_operators <- c('Avanti West Coast', 
                  'CrossCountry', 
                  'East Midlands Railway', 
                  'Northern Trains', 
                  'TransPennine Express')

operators_train_canc <- train_canc %>%
  filter(`National or Operator` %in% my_operators)

# Creating new Year variable as numeric

operators_train_canc <- operators_train_canc %>%
  mutate(
    Year = as.numeric(stringr::str_extract(`Time period`, "\\d{4}"))
  )

# putting it first

operators_train_canc <- operators_train_canc %>%
  select(Year, everything())

# Selecting only columns of arrival time

trains_arrv_canc <- operators_train_canc %>%
  select(`Year`, `National or Operator`, `Number of trains planned`, 
         `Cancellations`)

# selecting only columns of cancellation reason

trains_reason_canc <- operators_train_canc %>%
  select(`Year`, `National or Operator`, `Cancellations by responsibility, infrastructure and network management`, 
         `Cancellations by responsibility, infrastructure owner external event`,
         `Cancellations by responsibility, train operator fault`,
         `Cancellations by responsibility, operator external event`, `Number of trains planned`)

# adding percentage column

trains_reason_canc_perc <- trains_reason_canc %>%
  mutate(
    Percentage_canc_infrastucture_and_network_mngmt = (`Cancellations by responsibility, infrastructure and network management`
                            / `Number of trains planned`) * 100, 
Percentage_canc_infrastucture_owner_external_event = (`Cancellations by responsibility, infrastructure owner external event`
                                                        / `Number of trains planned`) * 100,
Percentage_canc_train_operator_fault = (`Cancellations by responsibility, train operator fault`
                                                          / `Number of trains planned`) * 100,
Percentage_canc_train_operator_external_event = (`Cancellations by responsibility, operator external event`
                                            / `Number of trains planned`) * 100)

# now selecting only the percentages

trains_reason_canc_perc <- trains_reason_canc_perc %>%
  select(`Year`, `National or Operator`, `Percentage_canc_infrastucture_and_network_mngmt`, 
         `Percentage_canc_infrastucture_owner_external_event`,
         `Percentage_canc_train_operator_fault`,
         `Percentage_canc_train_operator_external_event`)

# Calculating avrg for each year
avg_canc_reasons_per_year <- trains_reason_canc_perc %>%
  
  # 1. Group only by the Year
  group_by(Year, `National or Operator`) %>%
  
  # 2. Calculate the average for your new column
  summarize(
    Percentage_canc_infrastucture_and_network_mngmt = mean(Percentage_canc_infrastucture_and_network_mngmt,
                                                           na.rm = TRUE),
    Percentage_canc_infrastucture_owner_external_event = mean(Percentage_canc_infrastucture_owner_external_event,
                                                              na.rm = TRUE),
    Percentage_canc_train_operator_fault = mean(Percentage_canc_train_operator_fault, na.rm = TRUE),
    Percentage_canc_train_operator_external_event = mean(Percentage_canc_train_operator_external_event,
                                                         na.rm = TRUE)
  ) %>%
  
  ungroup()

# reshape the data

avg_canc_reasons_long <- avg_canc_reasons_per_year %>%
  pivot_longer(
    cols = -c(Year, `National or Operator`),
    names_to = "Cancellation_Reason",
    values_to = "Percentage",
    names_prefix = "Percentage canc_"     
  )

# want to create a dataset where all operators average is calculated to make a nice total line graph

avg_canc_rsns_long_oper <- avg_canc_reasons_long %>%
  group_by(Year, Cancellation_Reason) %>%
  summarise(
    Average_Percentage = mean(Percentage, na.rm = TRUE),
    .groups = "drop"
  )


#changing the names of reasons of cancellations to presentable names

# Clean the names in  dataframe FIRST
avg_canc_rsns_long_oper <- avg_canc_rsns_long_oper %>%
  mutate(Cancellation_Reason = case_match(Cancellation_Reason,
                                          "Percentage_canc_infrastucture_and_network_mngmt" ~ "Infrastucture and Network Management",
                                          "Percentage_canc_infrastucture_owner_external_event" ~ "Infrastucture Owner External Event",
                                          "Percentage_canc_train_operator_fault" ~ "Train Operator Fault",
                                          "Percentage_canc_train_operator_external_event" ~ "Train Operator External Event",
                                          .default = Cancellation_Reason 
  ))

# making the line graph

reason_line_graph<-ggplot(avg_canc_rsns_long_oper, 
       aes(x = Year, y = Average_Percentage, color = Cancellation_Reason)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Set2") + rs
  theme_minimal() +
  labs(title = "Trends in Average Cancellations by Reason", y="Average Percentage", 
       x= "Year",color = "Cancellation Reason") +
  theme_gray()+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
  )

# now looking at how many cancellations


# creating new percentage column

trains_arrv_canc <- trains_arrv_canc %>%
  mutate(
    Percentage_Cancelled = (`Cancellations` / `Number of trains planned`) * 100
  )
# Calculating avrg for each year
avg_cancellations_per_year <- trains_arrv_canc %>%
  
  # 1. Group only by the Year
  group_by(Year, `National or Operator`) %>%
  
  # 2. Calculate the average for your new column
  summarize(
    Avg_Percentage_Cancelled = mean(Percentage_Cancelled, na.rm = TRUE)
  ) %>%
  
  ungroup()

# bar chart total operators cancellations through the years

all_oper_time_bar_chart<-ggplot(data = avg_cancellations_per_year, 
       aes(x = factor(Year),
           y = Avg_Percentage_Cancelled)) +
  geom_col(fill = "plum4") + 
  labs(
    title = "Average Train Cancellations per Year (All Operators)",
    x = "Year",
    y = "Average Cancellation Percentage"
  ) +
  theme_gray()+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
  )


# collecting the data to create the average trend in cancellations bar chart

data_all_operators_trend <- trains_arrv_canc %>%
  
  filter(`National or Operator` %in% my_operators) %>%
  group_by(Year, `National or Operator`) %>%
  summarise(
    Average_Percentage = mean(Percentage_Cancelled, na.rm = TRUE), 
    .groups = "drop"
  )


#Creating the bar chart

all_oper_canc_bar <- ggplot(data = data_all_operators_trend, 
                            aes(x = factor(Year),       
                                y = Average_Percentage,  
                                fill = `National or Operator` 
                            )) + 
  geom_col(position = "dodge", width = 0.7) + 
  scale_fill_viridis_d(option = "magma", name = "Train Operator") +
  labs(
    title = "Average Trend in Train Cancellations: All Operators",
    x = "Year", 
    y = "Average Cancellation Percentage"
  ) +
  
  theme_grey() + 
  theme(
    legend.position = "top"
  )+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
  )

print(all_oper_canc_bar)


#Making a spider chart to compare years 2019 and 2024

data_comparison <- trains_arrv_canc %>%
  filter(Year %in% c(2019, 2024)) 

#changing the dataset to match how we make a spider chart
spider_data <- data_comparison %>%
  ungroup() %>%
  select(Year, `National or Operator`, Percentage_Cancelled) %>%
  pivot_wider(
    names_from = `National or Operator`, 
    values_from = Percentage_Cancelled,
    values_fn = mean
  ) %>%
  mutate(Year = as.character(Year))

# indicating max value
max_val <- max(spider_data[,-1], na.rm = TRUE) * 1.1 

#plot
spider_plot <- spider_data %>%
  ggradar(grid.min = 0,
    grid.mid = max_val / 2,
    grid.max = max_val,
    values.radar = c("0%", paste0(round(max_val/2, 1), "%"), paste0(round(max_val, 1), "%")),
     group.line.width = 1.5,              
    group.point.size = 3,               
    group.colours = c("cyan4", "darksalmon"), 
    fill = TRUE,
    fill.alpha = 0.15,  
    axis.label.size = 4,                
    grid.label.size = 4,   
    legend.position = "bottom"         
  ) +
  labs(title = "Train Cancellations by \nOperator: 2019 vs 2024") +
  theme(
    plot.title = element_text(hjust = 0.5,  size = 14)
  )+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
  )

print(spider_plot)


# arrange all four plots

composite_plot <- (reason_line_graph + all_oper_time_bar_chart) / (spider_plot + 
                                                                     all_oper_canc_bar) 

composite_plot

