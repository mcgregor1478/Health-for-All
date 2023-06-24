#1: Load required libraries
library(dplyr) 
library(ggplot2)

#2: Read in the health-related data
health_data <- read.csv("health-data.csv", header = TRUE)

#3: Process the data
processed_health_data <- health_data %>% 
  group_by(Disease) %>% 
  summarise(Avg_Age_At_Diagnosis = mean(Age_At_Diagnosis), 
            No_Of_Cases = n())

#4: Visualize the data
ggplot(processed_health_data, aes(x=Disease, y= No_Of_Cases)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  xlab("Disease") + 
  ylab("Number of Cases") + 
  ggtitle("Distribution of Health Cases by Disease (Overall)")

#5: Calculate disease-specific information
health_stats_by_disease <- health_data %>% 
  group_by(Disease) %>% 
  summarise(No_Of_Cases = n(),
            Avg_Age_At_Diagnosis = mean(Age_At_Diagnosis), 
            Avg_Cost_Per_Patient = mean(Cost_Per_Patient))
           
#6: Identify high-cost diseases
high_cost_diseases <- health_stats_by_disease %>% 
  filter(Avg_Cost_Per_Patient > 20000)

#7: Visualize high-cost diseases
ggplot(high_cost_diseases, aes(x=Disease, y=No_Of_Cases)) + 
  geom_bar(stat = "identity", fill = "red") +
  xlab("Disease") + 
  ylab("Number of Cases") + 
  ggtitle("Distribution of High-Cost Diseases")

#8: Calculate region-specific information
health_stats_by_region <- health_data %>% 
  group_by(Region) %>% 
  summarise(No_Of_Cases = n(),
            Avg_Age_At_Diagnosis = mean(Age_At_Diagnosis), 
            Avg_Cost_Per_Patient = mean(Cost_Per_Patient))

#9: Identify high-cost regions
high_cost_regions <- health_stats_by_region %>% 
  filter(Avg_Cost_Per_Patient > 20000)

#10: Visualize high-cost regions
ggplot(high_cost_regions, aes(x=Region, y=No_Of_Cases)) + 
  geom_bar(stat = "identity", fill = "purple") +
  xlab("Region") + 
  ylab("Number of Cases") + 
  ggtitle("Distribution of High-Cost Regions")
  
#11: Identify most expensive region
most_expensive_region <- health_stats_by_region %>% 
  arrange(desc(Avg_Cost_Per_Patient)) %>% 
  slice(1)

#12: Print the name of the most expensive region
cat("The most expensive region is:", most_expensive_region$Region)

#13: Calculate overall statistics
overall_health_stats <- health_data %>% 
  summarise(No_Of_Cases = n(), 
            Avg_Age_At_Diagnosis = mean(Age_At_Diagnosis), 
            Total_Cost_Per_Patient = sum(Cost_Per_Patient))

#14: Visualize overall data
ggplot(data = health_data, aes(x = Disease)) + 
  geom_bar(stat = "count", fill = "skyblue") + 
  xlab("Disease") + 
  ylab("Number of Cases") + 
  ggtitle("Distribution of Health Cases by Disease (Overall)")

#15: Calculate stats related to gender
gender_stats <- health_data %>% 
  group_by(Gender) %>% 
  summarise(No_Of_Cases = n(),
            Avg_Age_At_Diagnosis = mean(Age_At_Diagnosis), 
            Total_Cost_Per_Patient = sum(Cost_Per_Patient))

#16: Visualize stats related to gender
ggplot(data = gender_stats, aes( x = Gender, y = No_Of_Cases )) + 
  geom_bar(stat = "identity", fill = "green") + 
  xlab("Gender") + 
  ylab("Number of Cases") + 
  ggtitle("Distribution of Health Cases by Gender")

#17: Calculate the average age of diagnosis by gender
avg_age_stats <- health_data %>% 
  group_by(Gender) %>% 
  summarise(Avg_Age_At_Diagnosis = mean(Age_At_Diagnosis))

#18: Visualize the average age of diagnosis by gender
ggplot(data = avg_age_stats, aes( x = Gender, y = Avg_Age_At_Diagnosis)) + 
  geom_bar(stat = "identity", fill = "brown") + 
  xlab("Gender") + 
  ylab("Average Age at Diagnosis") + 
  ggtitle("Average Ages at Diagnosis by Gender")

#19: Calculate the average cost of treatment by gender
avg_cost_stats <- health_data %>% 
  group_by(Gender) %>% 
  summarise(Avg_Cost_Per_Patient = mean(Cost_Per_Patient))

#20: Visualize the average cost of treatment by gender
ggplot(data = avg_cost_stats, aes( x = Gender, y = Avg_Cost_Per_Patient)) + 
  geom_bar(stat = "identity", fill = "orange") + 
  xlab("Gender") + 
  ylab("Average Cost Per Patient") + 
  ggtitle("Average Costs Per Patient by Gender")

#21: Calculate the number of cases per region
region_stats <- health_data %>% 
  group_by(Region) %>% 
  summarise(No_Of_Cases = n())

#22: Visualize the number of cases per region
ggplot(data = region_stats, aes( x = Region, y = No_Of_Cases)) + 
  geom_bar(stat = "identity", fill = "pink") + 
  xlab("Region") + 
  ylab("Number of Cases") + 
  ggtitle("Number of Cases By Region")

#23: Calculate the average cost per case in each region
cost_per_case_stats <- health_data %>% 
  group_by(Region) %>% 
  summarise(Avg_Cost_Per_Patient = mean(Cost_Per_Patient))

#24: Visualize the average cost per case in each region
ggplot(data = cost_per_case_stats, aes( x = Region, y = Avg_Cost_Per_Patient)) + 
  geom_bar(stat = "identity", fill = "blue") + 
  xlab("Region") + 
  ylab("Average Cost Per Patient") + 
  ggtitle("Average Costs Per Patient By Region")

#25: Calculate the number of cases for each disease
disease_stats <- health_data %>% 
  group_by(Disease) %>% 
  summarise(No_Of_Cases = n())

#26: Visualize the number of cases for each disease
ggplot(data = disease_stats, aes( x = Disease, y = No_Of_Cases)) + 
  geom_bar(stat = "identity", fill = "black") + 
  xlab("Disease") + 
  ylab("Number of Cases") + 
  ggtitle("Number of Cases By Disease")

#27: Calculate the average cost per case for each disease
cost_per_case_disease_stats <- health_data %>% 
  group_by(Disease) %>% 
  summarise(Avg_Cost_Per_Patient = mean(Cost_Per_Patient))

#28: Visualize the average cost per case for each disease
ggplot(data = cost_per_case_disease_stats, aes( x = Disease, y = Avg_Cost_Per_Patient)) + 
  geom_bar(stat = "identity", fill = "yellow") + 
  xlab("Disease") + 
  ylab("Average Cost Per Patient") + 
  ggtitle("Average Costs Per Patient By Disease")

#29: Calculate total cost for each country
country_stats <- health_data %>% 
  group_by(Country) %>% 
  summarise(Total_Costs = sum(Cost_Per_Patient))

#30: Visualize the total cost for each country
ggplot(data = country_stats, aes( x = Country, y = Total_Costs)) + 
  geom_bar(stat = "identity", fill = "indigo") + 
  xlab("Country") + 
  ylab("Total Costs") + 
  ggtitle("Total Costs By Country")

#31: Calculate total number of cases for each country
cases_stats <- health_data %>% 
  group_by(Country) %>% 
  summarise(No_Of_Cases = n())

#32: Visualize the total number of cases for each country
ggplot(data = cases_stats, aes( x = Country, y = No_Of_Cases)) + 
  geom_bar(stat = "identity", fill = "violet") + 
  xlab("Country") + 
  ylab("Total Number of Cases") + 
  ggtitle("Total Number of Cases By Country")

#33: Calculate total cost for each age group
age_stats <- health_data %>% 
  group_by(Age_Group) %>% 
  summarise(Total_Costs = sum(Cost_Per_Patient))

#34: Visualize the total cost for each age group
ggplot(data = age_stats, aes( x = Age_Group, y = Total_Costs)) + 
  geom_bar(stat = "identity", fill = "gold") + 
  xlab("Age Group") + 
  ylab("Total Costs") + 
  ggtitle("Total Costs By Age Group")

#35: Calculate total number of cases for each age group
cases_age_stats <- health_data %>% 
  group_by(Age_Group) %>% 
  summarise(No_Of_Cases = n())

#36: Visualize the total number of cases for each age group
ggplot(data = cases_age_stats, aes( x = Age_Group, y = No_Of_Cases)) + 
  geom_bar(stat = "identity", fill = "cyan") + 
  xlab("Age Group") + 
  ylab("Number of Cases") + 
  ggtitle("Number of Cases By Age Group")

#37: Calculate the total cost for all health cases
all_stats <- health_data %>% 
  summarise(Total_Costs = sum(Cost_Per_Patient))

#38: Print the total cost for all health cases
cat("The total cost for all health cases is:", all_stats$Total_Costs)

#39: Calculate the average cost per case for all health cases
all_avg_cost_stats <- health_data %>% 
  summarise(Avg_Cost_Per_Patient = mean(Cost_Per_Patient))

#40: Print the average cost per case for all health cases
cat("The average cost per case for all health cases is:", all_avg_cost_stats$Avg_Cost_Per_Patient)

#41: Calculate the number of cases
all_cases_stats <- health_data %>% 
  summarise(No_Of_Cases = n())

#42: Print the number of cases
cat("The total number of cases is:", all_cases_stats$No_Of_Cases)

#43: Calculate the average age of diagnosis for all health cases
all_age_stats <- health_data %>% 
  summarise(Avg_Age_At_Diagnosis = mean(Age_At_Diagnosis))

#44: Print the average age of diagnosis for all health cases
cat("The average age of diagnosis for all health cases is:", all_age_stats$Avg_Age_At_Diagnosis)

#45: Calculate the proportion of cases in each age group
age_proportion_stats <- health_data %>% 
  group_by(Age_Group) %>% 
  summarise(Percentage_Of_Cases = n() / sum(n()) * 100)

#46: Visualize the proportion of cases in each age group
ggplot(data = age_proportion_stats, aes( x = Age_Group, y = Percentage_Of_Cases)) + 
  geom_bar(stat = "identity", fill = "magenta") + 
  xlab("Age Group") + 
  ylab("Percentage of Cases (%)") + 
  ggtitle("Proportion of Cases By Age Group")

#47: Calculate the proportion of cases in each gender group
gender_proportion_stats <- health_data %>% 
  group_by(Gender) %>% 
  summarise(Percentage_Of_Cases = n() / sum(n()) * 100)

#48: Visualize the proportion of cases in each gender group
ggplot(data = gender_proportion_stats, aes( x = Gender, y = Percentage_Of_Cases)) + 
  geom_bar(stat = "identity", fill = "turquoise") + 
  xlab("Gender") + 
  ylab("Percentage of Cases (%)") + 
  ggtitle("Proportion of Cases By Gender")

#49: Calculate the proportion of cases in each region
region_proportion_stats <- health_data %>% 
  group_by(Region) %>% 
  summarise(Percentage_Of_Cases = n() / sum(n()) * 100