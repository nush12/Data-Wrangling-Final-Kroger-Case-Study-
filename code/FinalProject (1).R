
# Fetching the Working directory -----------------------------------------------

getwd()


# Loading the desired libraries -------------------------------------------

library(readr)

library(lubridate)
library(stringr)
library(DT)
library(tidyverse)

library(plotly)
library(ranger)
library(zoo)

library(arules) # frequent items and association rules
library(arulesViz) # viz techniques for association and rules and item-sets
library(knitr) # dynamic report generation
library(plyr) # tools for splitting, applying and combining data
library(RColorBrewer)
library(MASS)
library(dplyr)


# Column Names ---------------------------------------------------

household_colnames <- c("HSHD_NUM","LOYALTY_FLAG","AGE_RANGE","MARITAL_STATUS",
                        "INCOME_RANGE","HOMEOWNER_DESC","HSHD_COMPOSITION",
                        "HH_SIZE","CHILDREN")
products_colnames <- c("PRODUCT_NUM","DEPARTMENT","COMMODITY","BRAND_TYPE",
                        "ORGANIC_FLAG")
transactions_colnames <- c("BASKET_NUM","HSHD_NUM","PURCHASE_DATE","PRODUCT_NUM",
                        "SPEND","UNITS","STORE_REGION",
                        "WEEK_NUM","YEAR")





files <- list("data/5000_households.csv", "data/5000_products.csv", 
              "data/5000_transactions.csv")
column_list <- list(household_colnames, products_colnames, transactions_colnames)
column_type <- list("ccccccccc", "ccccc", "ccccdicii")
skip_rows <- list(1, 1, 1)


df_combined <- pmap(list(file = files, col_names = column_list, col_types = column_type, 
                 skip = skip_rows), read_csv)


households <- data.frame(df_combined[[1]])
products <- data.frame(df_combined[[2]])
transactions <- data.frame(df_combined[[3]])

# Investigating all three dataset ---------------------------------------


households[households == ""] <- NA
products[products == ""] <- NA
transactions[transactions == ""] <- NA


map(df_combined,dim)
map(df_combined,names)
map(df_combined,head)
map(df_combined,tail)
map(df_combined,glimpse)
map(df_combined,~any(is.na(.)))
map(df_combined,~colSums(is.na(.)))
map(df_combined,summary)




# Replacing blank cells with NAs ---------------------------------------

for (i in 1:ncol(households)) {
  households[,i][households[,i ] == "null" | households[,i ] == "Unknown" | 
                   households[,i ] == "NOT AVAILABLE" ] <- NA
  
}

sum(is.na(households))
sum(complete.cases(households))




# Household Type Conversions --------------------------------------------------------


for (i in c(2,4,6,7)) {

  households[,i] <- as.factor(households[,i])
}



unique(households$INCOME_RANGE)
households$INCOME_RANGE <- factor(households$INCOME_RANGE, 
                                       order = TRUE, levels = 
                                        c("UNDER 35K","35-49K","50-74K","75-99K",
                                          "100-150K","150K+"))
households$AGE_RANGE <- str_trim(households$AGE_RANGE)
unique(households$AGE_RANGE)
households$AGE_RANGE <- factor(households$AGE_RANGE, 
                                  order = TRUE, levels = 
                                    c("19-24","25-34","35-44","45-54",
                                      "55-64","65-74","75+"))
households$CHILDREN <- factor(households$CHILDREN, 
                                  order = TRUE, levels = 
                                    c("1","2","3+"))
households$HH_SIZE <- factor(households$HH_SIZE, 
                              order = TRUE, levels = 
                                c("1","2","3","4","5+"))

# Products Type Conversions -----------------------------------------------


for (i in 2:5) {
  
  products[,i] <- as.factor(products[,i])
}


# Transactions Type Conversions -------------------------------------------

transactions$STORE_REGION <- as.factor(transactions$STORE_REGION)
transactions$PURCHASE_DATE <- dmy(transactions$PURCHASE_DATE)
transactions$BASKET_NUM <- as.numeric(transactions$BASKET_NUM)

 
# Merging the datasets ----------------------------------------



merged_data <- transactions %>% 
  inner_join(products, by = "PRODUCT_NUM") %>%
  inner_join(households, by = "HSHD_NUM")




# Market Basket Analysis --------------------------------------------------

#transactionData <- ddply(merged_data,c("BASKET_NUM","PURCHASE_DATE"),
#                       function(x)paste(x$COMMODITY, collapse = ","))
                                                                   
#transactionData$BASKET_NUM <- NULL
#transactionData$PURCHASE_DATE <- NULL
#colnames(transactionData) <- c("Items")
 
#write.csv(transactionData,"transactions.csv",quote=FALSE, row.names = FALSE)

tr <- read.transactions("transactions.csv",format = "basket", sep = ",")
class(tr)
summary(tr)

itemFrequencyPlot(tr,topN = 20,type = "relative",col = brewer.pal(8,'Pastel2'), 
                  main = "Absolute Item Frequency Plot")

association_rules <- apriori(tr, parameter = list(supp = 0.15, conf = 0.8,maxlen = 10,
                                                  target="rules"))
summary(association_rules)

inspect(association_rules[1:10])


subRules <- association_rules[quality(association_rules)$confidence > 0.8]

top10subRules <- head(association_rules, n=10, by="confidence")

plot(top10subRules, method = "graph",  engine = "htmlwidget")




# subRules2<-head(subRules, n=20, by="lift")

# plot(subRules2, method="paracoord")



# RFM Analysis ------------------------------------------------------------

purchase_year <- as.numeric(merged_data$PURCHASE_DATE %>% str_sub(start = 1, end = 4))
merged_data <- merged_data %>% mutate(Year = purchase_year)


recent_date <- max(merged_data$PURCHASE_DATE) + 1



df_RFM <- merged_data %>% 
  filter(Year == 2017) %>%
  group_by(HSHD_NUM) %>% 
  dplyr::summarise(Recency = as.numeric(recent_date - max(PURCHASE_DATE)),
            Frequency = length(unique(BASKET_NUM)), 
            MonetaryValue = sum(SPEND)) 
  
df_RFM[,-1] %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_density() + 
  theme_minimal() 

df_RFM$ScaledRecency <- scale(log(df_RFM$Recency))
df_RFM$ScaledFrequency <- scale(log(df_RFM$Frequency))
df_RFM$ScaledMonetaryValue <- scale(log(df_RFM$MonetaryValue))





wss <- vector("numeric",15)
for (i in 1:15) {
  km_out <- kmeans(df_RFM[,5:7], centers = i, nstart = 20, iter.max = 50)
  wss[i] <- km_out$withinss
}

plot(1:15,wss,type = "b", xlab = 'Number of Clusters', 
     ylab = "Within group of sum of squares")


km_out1 <- kmeans(df_RFM[,5:7], centers = 3, nstart = 20, iter.max = 50)




clustered_df <- data.frame(HSHD_NUM = df_RFM$HSHD_NUM, Recency = df_RFM$Recency, 
                           Frequency = df_RFM$Frequency, 
                           MonetaryValue = df_RFM$MonetaryValue, Cluster = km_out1$cluster)

summary_table <- clustered_df %>% 
  dplyr::select(Cluster, Recency,Frequency,MonetaryValue) %>%
  group_by(Cluster) %>% 
  dplyr::summarise(Recency = mean(Recency), Frequecy = mean(Frequency), 
                   Monetaryvalue = mean(MonetaryValue), 
                   Number_Household = length(Cluster))
  
gathered_table <- gather(summary_table[1:4], RFM, Mean_value, -Cluster)

ggplot(gathered_table, aes(y = Mean_value, x = RFM, color = as.factor(Cluster))) +
  geom_line(aes(group = Cluster)) + 
  scale_y_log10() + 
  labs(x = "RFM",
       y = "Mean RFM Value",
       title = "K-Means Clutering based on RFM")


# Stepwise Selection  -----------------------------------------------------

linear_data <- merged_data %>% 
  dplyr::select(HSHD_NUM,AGE_RANGE,MARITAL_STATUS,
                               INCOME_RANGE,HSHD_COMPOSITION,HH_SIZE,
                               CHILDREN, SPEND, LOYALTY_FLAG, UNITS, 
                               STORE_REGION, HOMEOWNER_DESC) %>% 
  group_by(HSHD_NUM) %>% 
  dplyr::summarise(SPEND = sum(SPEND),
            INCOME_RANGE = unique(INCOME_RANGE),LOYALTY_FLAG = unique(LOYALTY_FLAG),
            AGE_RANGE = unique(AGE_RANGE),
            CHILDREN = unique(CHILDREN), HOMEOWNER_DESC = unique(HOMEOWNER_DESC),
            MARITAL_STATUS = unique(MARITAL_STATUS), 
            HSHD_COMPOSITION = unique(HSHD_COMPOSITION), HH_SIZE = unique(HH_SIZE),
            UNITS = sum(UNITS))

linear_data <- linear_data[,-1]

map_dbl(linear_data, function(x){sum(is.na(x))}) %>% 
  sort(decreasing = TRUE) %>% .[. > 0]

linear_data <- linear_data[,-5]

entire_model <- lm(SPEND~., data = na.omit(linear_data))
null_model <- lm(SPEND~1, data = na.omit(linear_data))



stepwise_selection_full <- step(null_model, scope = list(lower = null_model
                                                         ,upper = entire_model),
                                direction = "both")


best_model <- lm(SPEND~ INCOME_RANGE + UNITS + HOMEOWNER_DESC + MARITAL_STATUS, 
                 data = na.omit(linear_data))

summary(best_model)


# Cohort Analysis ---------------------------------------------------------

cohort_data <- merged_data  %>% 
  dplyr::select(HSHD_NUM,PURCHASE_DATE,Year)

cohort_data$PURCHASE_DATE <- ymd(format(cohort_data$PURCHASE_DATE, "%Y-%m-01"))

Join_Date <- cohort_data %>%
     group_by(HSHD_NUM) %>% 
    dplyr::summarise(Joindate = min(PURCHASE_DATE))
    



cohort_data <- left_join(cohort_data,Join_Date, by = "HSHD_NUM")

years_diff <- as.numeric(format(cohort_data$PURCHASE_DATE,'%Y')) - 
                 as.numeric(format(cohort_data$Joindate,'%Y'))

months_diff <- as.numeric(format(cohort_data$PURCHASE_DATE,'%m')) - 
                    as.numeric(format(cohort_data$Joindate,'%m'))

cohort_data$monthindex <- years_diff * 12 + months_diff + 1

cohort_data$JoinMonth <- format(cohort_data$Joindate,"%Y-%m")


cohort_final <- cohort_data %>% group_by(JoinMonth, monthindex) %>%
  dplyr::summarise(hshd = length(unique(HSHD_NUM)))

cohort_spread <- spread(cohort_final,monthindex,hshd)

for (i in 3:25) {
  
cohort_spread[,i] <- round(((cohort_spread[,i] / cohort_spread[,2]) * 100), 1)

}

cohort_spread[,2] <- round(((cohort_spread[,2] / cohort_spread[,2]) * 100),1)

cohort_spread <- cohort_spread[-1,]

breaks <- quantile(data.frame(cohort_spread[,2:25]), probs = seq(.05, .95, .05), 
                   na.rm = TRUE)
colors2 <- sapply(round(seq(255, 40, length.out = length(breaks) + 1), 0),
                  function(x){ rgb(255,x,x, maxColorValue = 255) } )


# The Retention Mixpanel with counts
DT::datatable(cohort_spread,
              class = 'cell-border stripe',
              rownames = FALSE,
              options = list(
                ordering=F,
                dom = 't',
                pageLength = 23) ) %>%
  formatStyle("JoinMonth",
              backgroundColor = 'lightgrey',
              fontWeight = 'bold') %>%
  formatStyle(names(cohort_spread[-1]),fontWeight = 'bold',
              color = 'white', backgroundColor = styleInterval(breaks,colors))


# income and spending -----------------------------------------------------



spending <- merged_data %>% 
  dplyr::select(HSHD_NUM, SPEND, INCOME_RANGE, UNITS, LOYALTY_FLAG, 
                AGE_RANGE,MARITAL_STATUS, HOMEOWNER_DESC,
                HSHD_COMPOSITION, HH_SIZE, CHILDREN) %>%
  group_by(HSHD_NUM) %>% 
  dplyr::summarise(TOT_SPEND = sum(SPEND), INCOME_RANGE = unique(INCOME_RANGE), 
            TOT_UNITS  = sum(UNITS), LOYALTY_FLAG = unique(LOYALTY_FLAG), 
            AGE_RANGE = unique(AGE_RANGE), MARITAL_STATUS = unique(MARITAL_STATUS), 
            HOMEOWNER_DESC = unique(HOMEOWNER_DESC), 
            HSHD_COMPOSITION = unique(HSHD_COMPOSITION),
            HH_SIZE = unique(HH_SIZE), CHILDREN = unique(CHILDREN))




spend_plot <- ggplot(spending, aes(y = TOT_SPEND, x = TOT_UNITS
                        , color = INCOME_RANGE)) +
  geom_point(alpha = 0.5, position = "jitter") +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(x = "Total Units",
              y = "Total Spending of individual households",
              title = "Income wise Household Spending - 2016 & 2017")

ggplotly(spend_plot)
  
# Time Series -------------------------------------------------------------


purchase_yemo <- merged_data$PURCHASE_DATE %>% str_sub(start = 1, end = 7)

merged_data <- merged_data %>% mutate(YeMo = purchase_yemo)
  
spending_timeseries <- merged_data %>% 
  dplyr::select(SPEND, YeMo, Year) %>%
  group_by(YeMo) %>% 
  dplyr::summarise(tot_spend = sum(SPEND), YE = unique(Year))

timeseries_plot <- ggplot(spending_timeseries, aes(y = tot_spend, 
                    x = as.Date(as.yearmon(YeMo)),
                    color = as.factor(YE))) +
  geom_point(alpha = 0.5, position = "jitter") +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1, col = "All"), linetype = 2) +
  labs(x = "Monthly Index",
       y = "Monthwise Spending",
       title = "Household Spending month by month- 2016 & 2017", 
       color = "Year")

ggplotly(timeseries_plot)


# Organic, Non-organic ----------------------------------------------------


spending_timeseries_org <- merged_data %>% 
  dplyr::select(SPEND, YeMo, YEAR, ORGANIC_FLAG) %>% 
  group_by(YeMo, ORGANIC_FLAG) %>% 
  dplyr::summarise(tot_spend = sum(SPEND), YE = unique(YEAR))

org_plot <- ggplot(spending_timeseries_org, aes(y = tot_spend, 
                                                x = as.Date(as.yearmon(YeMo)),
                                                color = as.factor(YE))) +
  geom_point(alpha = 0.5, position = "jitter") +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(. ~ ORGANIC_FLAG, scales = "free_y") +
  labs(x = "Monthly Index",
       y = "Monthwise Spending",
       title = "Monthly spending for orgainic products and non-organic products", 
       color = "Year")


ggplotly(org_plot)

# Spending histogram ------------------------------------------------------



#Spend_fill <- scale_fill_brewer("INCOME_RANGE",palette = "Greens")

#spending2 <- na.omit(spending)

#spend_plot <- ggplot(spending2, aes(x = TOT_SPEND, fill = INCOME_RANGE)) +
#  geom_histogram(aes(y = ..count../sum(..count..)), position = "fill") +
# Spend_fill + 
# labs(x = "Household Spending",
#      y = "Proportion of Spending",
 #    title = "Income Wise Household Spending", 
#     color = "Income Range")

#ggplotly(spend_plot)




# Spending Region Wise histogram -----------------------------------------------

summar <- merged_data %>% 
  dplyr::select(HSHD_NUM, SPEND, Year, STORE_REGION) %>%
  group_by(STORE_REGION, HSHD_NUM, Year) %>% 
  dplyr::summarise(tot_spend = sum(SPEND)) 
  

gg_density <- ggplot(summar,aes(x = tot_spend, fill = STORE_REGION)) + 
  geom_density(col = NA, alpha = 0.35) +
  labs(x = "Household Spending",
       y = "Density",
       title = "Region Wise Spending", 
       color = "Store Region")

ggplotly(gg_density)


# Commodity-Yearly Spending -----------------------------------------------



summar2 <- merged_data %>% 
  dplyr::select(COMMODITY, SPEND, Year) %>%
  group_by(COMMODITY, Year) %>% 
  dplyr::summarise(tot_spend = sum(SPEND))

summar2$Year <- as.factor(summar2$Year)

cc <- ggplot(summar2, aes(x = tot_spend, y = COMMODITY, col = Year)) +
  geom_point(scale = "free_y", space = "free_y", position = "jitter", alpha = 0.4) +
  labs(x = "Spending",
       y = "Commodity Type",
       title = "Yearly Spending commodity wise", 
       color = "Year")

ggplotly(cc)

















