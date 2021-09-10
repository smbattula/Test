library(tidyverse)
library(xlsx)

#Q1
texasGas <- read.xlsx("texasGas.xlsx",1) #working directory contains file to be read
View(texasGas)

#a.(5 pts) Convert the month column to ordered factor such that January < February < …. < December.
texasGas$Month <- factor(texasGas$Month, ordered = TRUE,levels = c("January","February","March", "April","May","June","July","August","September","October", "November", "December"))
str(texasGas)

#b. (10 pts) In order to have one observation in each row, apply pivot_longer function to the data frame. Also, convert the Rating column to ordered factor such that Regular <   Midgrade < Premium.
texasGasLong <- pivot_longer(texasGas, cols = c(Regular, Midgrade,Premium),names_to = "Rating", values_to = "Price", values_drop_na = TRUE)
texasGasLong$Rating = factor(texasGasLong$Rating, ordered = TRUE,levels = c( "Regular","Midgrade", "Premium"))
str(texasGasLong)

#c. (15 pts) Create the following point plot using faceting.
ggplot(texasGasLong, aes(x=Month, y=Price, color=Rating),na.rm = TRUE) + geom_point() + facet_wrap(~Year)

#d. (10 pts) Unite the month and year columns into one date column.
texasGasLong <- unite(texasGasLong, col = "Date", c(Month, Year), sep = " ")

#e. (15 pts) Using dplyr functions, print the first five months having minimum “Premium” gas price as follows:
texasGasLong %>%  filter(Rating=="Premium") %>%arrange(Price) %>% select(-Rating)  %>% head(5) %>% print()

#f. (10 pts) Separate the date column into month and year columns.
texasGasLong <- separate(texasGasLong, col = Date, into=c("Month","Year"), sep = " ")
texasGasLong

#g. (10 pts) Widen your data by applying pivot_wider function. At the end, you should have the initial data  
texasGasWide <- pivot_wider(texasGasLong, names_from = "Rating", values_from = "Price")
texasGasWide

#h. (25 pts) Write a function named MultipleOf3or4 that takes an integer and 
#returns true if that number is divisible by 3 or 4, returns false otherwise. 
#Create a vector of 10 random #integers from 1 to 100. Apply the function 
#MultipleOf3or4 to all numbers in your vector #to check whether they are 
#divisible by 3 or 4 using map_lgl function in purrr package.For instance, 
#if the random numbers are [76 22 38 45 39 88 61 78 83 95], the result 
#should be [TRUE FALSE FALSE TRUE TRUE TRUE FALSE TRUE FALSE FALSE].

MultipleOf3or4 <- function(x){
  if(x%%3==0 || x%%4==0){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

intVec <- sample(1:100, 10)
intVec
map_lgl(intVec, MultipleOf3or4)
