
# Set preferences for conflicted functions
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

install.packages("dplyr")
install.packages("tidyverse")
install.packages("stringr")
install.packages("pdftools")
install.packages("conflicted")
library(conflicted)
library(pdftools)
library(stringr)
library(dplyr)
library(tidyverse)
library(tidyr)

default_locale()$date_names$mon

tr_locale <- default_locale()

tr_months <- c('Ocak', 'Şubat', 'Mart','Nisan', 'Mayıs', 'Haziran',
                'Temmuz', 'Ağustos', 'Eylül','Ekim', 'Kasım', 'Aralık')

tr_locale$date_names$mon <- tr_months
tr_locale$date_names$mon


streamflow <- pdf_text("E:/IST5128/Final_Project/STREAMFLOW.pdf")

page1 <- streamflow[[1]]
page1 <- strsplit(page1, "\n") %>% unlist

#Q1

pattern <- "(D\\d{2}[A-Z]\\d{3})\\s+(.*)$"

Q1 <- str_match(page1[[2]], pattern) %>%
  na.omit() %>%
  as.data.frame() %>%
  rename("Station-Code_Name" = 1, "Station-Code" = 2, "Station-Name" = 3)

Q1

#Q2
pattern <- "(\\d+°\\d+'\\d+\"\\s+(Doğu|Batı|Güney|Kuzey)\\s-\\s\\d+°\\d+'\\d+\")"

Q2 <- page1 %>% 
  str_subset(pattern) %>%
  str_trim()

Q2

#Q3
Q3 <- page1 %>% str_which("Gün")
Q3

#Q4

line_of_years <- page1[Q3 - 2]

years <- str_extract_all(line_of_years, "\\d{4}")

if (length(years[[1]]) == 2) {
  start_year <- as.numeric(years[[1]][1])
  end_year <- as.numeric(years[[1]][2])
} else {
  start_year <- NA
  end_year <- NA
}

Q4 <- data.frame(start_year = start_year, end_year = end_year)

Q4

#Q5a
table_lines <- page1[(Q3 + 1):(Q3 + 31)]

table_lines <- str_trim(table_lines)

Q5a <- data.frame(table_lines)

Q5a

#Q5b
table_lines <- page1[c(Q3,(Q3 + 2):(Q3 + 32))]

table_lines <- str_trim(table_lines)

Q5b <- data.frame(table_lines)

Q5b

#Q5c
Q5c <- str_split_fixed(Q5b$table_lines, "\\s+", 13)

Q5c

#Q5d

Q5d <- apply(Q5c, 2, function(column) {
  column[column == "------" | column == "KURU"] <- ""
  return(column)
})

Q5d

#Q5e

Q5e <- Q5d[-3, ]
colnames(Q5e) <- Q5d[1, ]
Q5e <- as.data.frame(Q5e[-1, ])
Q5e <- apply(Q5e, 2, as.numeric)

Q5e

#Q5f
start_year <- Q4$start_year
end_year <- Q4$end_year

current_col_names <- colnames(Q5e)

updated_col_names <- c("Gün",
                       paste(current_col_names[2:5], start_year, sep = " "),
                       paste(current_col_names[6:13], end_year, sep = " "))

colnames(Q5e) <- updated_col_names

Q5f <- Q5e
Q5f

#Q6
library(tidyverse)

Q5f_df <- as.data.frame(Q5f)

Q6 <- Q5f_df %>%
  pivot_longer(cols = -Gün, names_to = "name", values_to = "akım") %>%
  mutate(date = paste(Gün, name, sep = " ")) %>%
  mutate(date = parse_date(date, format = "%d %B %Y", locale = tr_locale)) %>% 
  dplyr::filter(!is.na(akım))

station_code1 <- Q1$`Station-Code`
station_name1 <- Q1$`Station-Name`

Q6$station_name <- station_name1
Q6$station_code <- station_code1

Q6 <- Q6 %>%
  mutate(long_lat = Q2)  

Q6 <- Q6 %>%
  select(-Gün, -name) 

head(Q6)

#Q7

extract_pdf <- function(pdf_file){
  
  streamflow <- pdf_text(pdf_file)
  
  return_table <- data.frame()
  
  for (i in 1:60) {
    page <- streamflow[[i]]
    
    page1 <- strsplit(page, "\n") %>% unlist
    
    pattern <- "(D\\d{2}[A-Z]\\d{3})\\s+(.*)$"
    
    Q1 <- str_match(page1[2], pattern) %>%
      na.omit() %>%
      as.data.frame() %>%
      rename("Station-Code_Name" = 1, "Station-Code" = 2, "Station-Name" = 3)
    
    pattern <- "(\\d+°\\d+'\\d+\"\\s+(Doğu|Batı|Güney|Kuzey)\\s-\\s\\d+°\\d+'\\d+\")"
    
    Q2 <- page1 %>% 
      str_subset(pattern) %>%
      str_trim()
    
    Q3 <- page1 %>% str_which("Gün")
    
    line_of_years <- page1[Q3 - 2]
    
    years <- str_extract_all(line_of_years, "\\d{4}")
    
    if (length(years[[1]]) == 2) {
      start_year <- as.numeric(years[[1]][1])
      end_year <- as.numeric(years[[1]][2])
    } else {
      start_year <- NA
      end_year <- NA
    }
    
    Q4 <- data.frame(start_year = start_year, end_year = end_year)
    
    table_lines <- page1[(Q3 + 1):(Q3 + 31)]
    
    table_lines <- str_trim(table_lines)
    
    Q5a <- data.frame(table_lines)
    
    table_lines <- page1[c(Q3,(Q3 + 2):(Q3 + 32))]
    
    table_lines <- str_trim(table_lines)
    
    Q5b <- data.frame(table_lines)
    
    Q5c <- str_split_fixed(Q5b$table_lines, "\\s+", 13)
    
    Q5d <- apply(Q5c, 2, function(column) {
      column[column == "------" | column == "KURU"] <- ""
      return(column)
    })
    
    Q5e <- Q5d[-3, ]
    colnames(Q5e) <- Q5d[1, ]
    Q5e <- as.data.frame(Q5e[-1, ])
    Q5e <- apply(Q5e, 2, as.numeric)
    
    start_year <- Q4$start_year
    end_year <- Q4$end_year
    
    current_col_names <- colnames(Q5e)
    
    updated_col_names <- c("Gün",
                           paste(current_col_names[2:5], start_year, sep = " "),
                           paste(current_col_names[6:13], end_year, sep = " "))
    
    colnames(Q5e) <- updated_col_names
    
    Q5f <- Q5e
    
    Q5f_df <- as.data.frame(Q5f)
    
    Q6 <- Q5f_df %>%
      pivot_longer(cols = -Gün, names_to = "name", values_to = "akim") %>%
      mutate(date = paste(Gün, name, sep = " ")) %>%
      mutate(date = parse_date(date, format = "%d %B %Y", locale = tr_locale)) %>% 
      dplyr::filter(!is.na(akim))
    
    Q6 <- Q6 %>%
      mutate(station_code = Q1$`Station-Code`,
             station_name = Q1$`Station-Name`,
             long_lat = Q2)
    
    Q6 <- Q6 %>%
      select(-Gün, -name) 
    
    return_table <- bind_rows(return_table, Q6)
  }
  
  return(return_table)
}

pdf_file <- pdf_text("E:/IST5128/Final_Project/STREAMFLOW.pdf")

flow_daily <- extract_pdf(pdf_file)

# (hata aldım çalışmadı)

#Q8

install.packages("openxlsx")
library(openxlsx)

flow_daily <- read.xlsx("E:/IST5128/Final_Project/flow_daily.xlsx")

Q8 <- flow_daily %>%
  group_by(station_name) %>%
  summarize(mean_flow = mean(akım, na.rm = TRUE)) %>%
  arrange(desc(mean_flow)) %>%
  head(10)

Q8

#Q9

Q9 <- flow_daily %>%
  group_by(station_name, date) %>%
  summarize(mean_flow = mean(akım)) %>%
  arrange(desc(mean_flow))

Q9 %>% head(20)

#Q10)

Q10 <- flow_daily
Q10$long_lat <- gsub("°|'", " ", Q10$long_lat)  
Q10$long_lat <- gsub("\"", " ", Q10$long_lat)   
Q10$long_lat <- gsub("Doğu", "E", Q10$long_lat)  
Q10$long_lat <- gsub("Kuzey", "N", Q10$long_lat)  
Q10$long_lat <- gsub("Batı", "W", Q10$long_lat)  
Q10$long_lat <- gsub("Güney", "S", Q10$long_lat)  


Q10 <- Q10 %>%
  mutate(
    longitude = substring(long_lat, 0, 11),
    latitude = substring(long_lat, regexpr("-", long_lat) + 1, regexpr("N", long_lat))
  )

Q10 %>% head()

#Q11
install.packages("parzer")
library(parzer)

Q11$parsed_lon <- parse_lon(Q11$longitude)
Q11$parsed_lat <- parse_lat(Q11$latitude)

Q11 %>% head()

#Q12

Q12 <- Q8 %>%
  left_join(Q11 %>% distinct(station_name, .keep_all = TRUE), by = "station_name")

Q12 <- Q12 %>% select(-station_code, -akım, -latitude, -longitude, -long_lat)

#Q13

install.packages("leaflet")
library(leaflet)

map <- leaflet(Q12) %>%
  addTiles() %>%
  addCircles(
    lng = ~parsed_lon,
    lat = ~parsed_lat,
    radius = ~mean_flow * 10,  
    popup = ~station_name,
    fillOpacity = 0.5
  )

print(map)
