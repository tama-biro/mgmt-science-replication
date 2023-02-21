library(tidyverse)


filepath <- './data/processed_data.csv'

data <- read.csv(filepath)



# Figure 1.1 - avg dist of project lifetime
data_plot1.1 <- data %>%
  filter(question_number == 1) %>%
  mutate(Choices = factor(Choices,
                          levels = c('less than 1 year',
                                     'between 1 and 3 years',
                                     'between 3 and 5 years',
                                     'between 5 and 7 years',
                                     'more than 7 years'),
                          labels = c('< 1 yr',
                                     '1 - 3yrs',
                                     '3 - 5yrs',
                                     '5 - 7yrs',
                                     '> 7 yrs')))

ggplot(data_plot1.1, aes(x = Choices, y = avg)) +
  geom_bar(stat = 'identity', fill = '#4472C4') +
  scale_y_continuous(name = '',
                     limits = c(0, 0.4),
                     breaks = seq(0, 0.4, 0.1)) +
  labs(x = '',
       title = 'Average Distribution of Project Lifetimes') +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())


# Figure 1.2 - dist probability renegotiate
data_plot1.2 <- data %>%
  filter(question_number == 5) %>%
  select(starts_with('Subject')) %>%
  t %>%
  as.data.frame %>%
  drop_na %>%
  mutate(x = if_else(V1 < .21, '0 - 21%',
                     if_else(V1 < .41, '21 - 40%',
                             if_else(V1 < .61, '41 - 60%',
                                     if_else(V1 < .81, '61 - 80%', '81 - 100%'))))) %>%
  group_by(x) %>%
  summarize(count = n()) %>%
  ungroup

ggplot(data_plot1.2, aes(x = x, y = count)) +
  geom_bar(stat = 'identity', fill = '#4472C4',
           width = 0.8) +
  scale_y_continuous(name = '',
                     limits = c(0, 10),
                     breaks = seq(0, 10, 2)) +
  labs(x = '',
       title = 'Distribution of Responses Concerning the
Probability to Renegotiate') +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())


# Figure 2 - Reasons why price negotiate
data_plot2 <- data %>%
  filter(question_number == 6) %>%
  mutate(Choices = factor(Choices,
                          levels = c('Change of cost of input factors',
                                     'Learning curve effects of the supplier',
                                     'New suppliers become available',
                                     'Design changes of the initial good')))

ggplot(data_plot2, aes(x = avg, y = Choices)) +
  geom_bar(stat = 'identity', fill = '#4472C4',
           width = 0.5) +
  scale_x_continuous(name = '',
                     limits = c(0, 0.6),
                     breaks = seq(0, 0.6, 0.1)) +
  labs(y = '',
       title = 'Average Frequency of Reasons for Why Initial Prices Are Renegotiated') +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank())

# Figure 3.1 - How frequent index could be used
data_plot3.1 <- data %>%
  filter(question_number == 4) %>%
  select(starts_with('Subject')) %>%
  t %>%
  as.data.frame %>%
  drop_na %>%
  mutate(x = if_else(V1 < .21, '0 - 21%',
                     if_else(V1 < .41, '21 - 40%',
                             if_else(V1 < .61, '41 - 60%',
                                     if_else(V1 < .81, '61 - 80%', '81 - 100%'))))) %>%
  group_by(x) %>%
  summarize(count = n()) %>%
  ungroup

ggplot(data_plot3.1, aes(x = x, y = count)) +
  geom_bar(stat = 'identity', fill = '#4472C4',
           width = 0.8) +
  scale_y_continuous(name = '',
                     limits = c(0, 8),
                     breaks = seq(0, 8, 1)) +
  labs(x = '',
       title = 'How Frequently an Index Could Have Been Used') +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Figure 3.2 - How frequent index was used
data_plot3.2 <- data %>%
  filter(question_number == 3) %>%
  select(starts_with('Subject')) %>%
  t %>%
  as.data.frame %>%
  drop_na %>%
  mutate(x = if_else(V1 < .21, '0 - 21%',
                     if_else(V1 < .41, '21 - 40%',
                             if_else(V1 < .61, '41 - 60%',
                                     if_else(V1 < .81, '61 - 80%', '81 - 100%'))))) %>%
  group_by(x) %>%
  summarize(count = n()) %>%
  ungroup

ggplot(data_plot3.2, aes(x = x, y = count)) +
  geom_bar(stat = 'identity', fill = '#4472C4',
           width = 0.8) +
  scale_y_continuous(name = '',
                     limits = c(0, 12),
                     breaks = seq(0, 12, 2)) +
  labs(x = '',
       title = 'How Frequently an Index Has Been Used') +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Figure 4 - How often supplier switched
data_plot4 <- data %>%
  filter(question_number == 2) %>%
  select(starts_with('Subject')) %>%
  t %>%
  as.data.frame %>%
  drop_na %>%
  mutate(x = if_else(V1 < .21, '0 - 21%',
                     if_else(V1 < .41, '21 - 40%',
                             if_else(V1 < .61, '41 - 60%',
                                     if_else(V1 < .81, '61 - 80%', '81 - 100%')))),
         x = factor(x, levels = c('0 - 21%',
                                  '21 - 40%',
                                  '41 - 60%',
                                  '61 - 80%',
                                  '81 - 100%'))) %>%
  group_by(x) %>%
  summarize(count = n()) %>%
  ungroup %>%
  complete(x,
           fill = list(count = 0))

ggplot(data_plot4, aes(x = x, y = count)) +
  geom_bar(stat = 'identity', fill = '#4472C4',
           width = 0.8) +
  scale_y_continuous(name = '',
                     limits = c(0, 16),
                     breaks = seq(0, 16, 2)) +
  labs(x = '',
       title = 'How Often
a Supplier Is Switched During the Lifetime of a Project') +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Figure 5 - Prevalence of auction formats
data_plot5 <- data %>%
  filter(question_number == 9) %>%
  mutate(Choices = factor(Choices,
                          levels = c('English auction',
                                     'Second-price sealed-bid auction',
                                     'Dutch auction',
                                     'First-price sealed-bid auction')))

ggplot(data_plot5, aes(x = avg, y = Choices)) +
  geom_bar(stat = 'identity', fill = '#4472C4',
           width = 0.5) +
  scale_x_continuous(name = '',
                     limits = c(0, 0.9),
                     breaks = seq(0, 0.9, 0.1)) +
  labs(y = '',
       title = 'Prevalence of Different Auction Formats in Procurement') +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank())

# Figure 6 - Importance of fixing prices in advance
data_plot6 <- data %>%
  filter(question_number == 7) %>%
  mutate(Choices = factor(Choices,
                          levels = c('Change of cost of input factors',
                                     'Learning curve effects of the supplier',
                                     'New suppliers become available',
                                     'Design changes of the initial good')))

ggplot(data_plot6, aes(x = avg, y = Choices)) +
  geom_bar(stat = 'identity', fill = '#4472C4',
           width = 0.5) +
  scale_x_continuous(name = '',
                     limits = c(0, 1),
                     breaks = seq(0, 1, 0.1)) +
  labs(y = '',
       title = 'Importance of Fixing Prices in Advance for Different Triggers of
Renegotiation') +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank())

