---
title: "P8130 Biostatistics Homework 4"
author: "Bowen Xia (UNI: bx2232)"
date: "November 25, 2025"
output:
  pdf_document:
    toc: true
    toc_depth: 3
    number_sections: true
    fig_caption: true
  html_document:
    toc: true
    toc_float: true
---



\newpage

# Problem 1: Blood Sugar Analysis (10 points)

## Problem Statement

A new blood sugar monitoring device is being evaluated. We have data from 25 patients with similar blood sugar distributions. We need to test whether there is significant evidence ($\alpha = 0.05$) that the median blood sugar reading is less than 120 in the population.

## Data


``` r
# Blood sugar data
blood_sugar <- c(125, 123, 117, 123, 115, 112, 128, 118, 124, 111, 116, 109, 125,
                 120, 113, 123, 112, 118, 121, 118, 122, 115, 105, 118, 131)

# Summary statistics
cat("Sample size:", length(blood_sugar), "\n")
```

```
## Sample size: 25
```

``` r
cat("Mean:", round(mean(blood_sugar), 2), "\n")
```

```
## Mean: 118.48
```

``` r
cat("Median:", round(median(blood_sugar), 2), "\n")
```

```
## Median: 118
```

``` r
cat("Standard deviation:", round(sd(blood_sugar), 2), "\n")
```

```
## Standard deviation: 6.19
```

``` r
cat("Range:", min(blood_sugar), "-", max(blood_sugar), "\n")
```

```
## Range: 105 - 131
```

## Part a) Sign Test

**Hypotheses:**
$$H_0: \text{median} = 120 \text{ vs. } H_a: \text{median} < 120$$

### Methodology

The sign test is a non-parametric test that examines whether the median differs from a hypothesized value. For each observation, we determine if it's above (+) or below (-) the hypothesized median. Under $H_0$, we expect equal numbers of + and - signs.


``` r
# Calculate differences from hypothesized median
hypothesized_median <- 120
differences <- blood_sugar - hypothesized_median

# Count positive, negative, and zero differences
n_positive <- sum(differences > 0)
n_negative <- sum(differences < 0)
n_zero <- sum(differences == 0)
n_nonzero <- n_positive + n_negative

cat("Number of values > 120:", n_positive, "\n")
```

```
## Number of values > 120: 10
```

``` r
cat("Number of values < 120:", n_negative, "\n")
```

```
## Number of values < 120: 14
```

``` r
cat("Number of values = 120:", n_zero, "\n")
```

```
## Number of values = 120: 1
```

``` r
cat("Sample size (excluding zeros):", n_nonzero, "\n\n")
```

```
## Sample size (excluding zeros): 24
```

``` r
# For one-sided test (Ha: median < 120)
# We want P(X >= n_positive) where X ~ Binomial(n_nonzero, 0.5)
# Equivalently, P(X <= n_negative) for lower tail
p_value_sign <- pbinom(n_negative, n_nonzero, 0.5)

cat("Sign Test Results:\n")
```

```
## Sign Test Results:
```

``` r
cat("  Test statistic (number of - signs):", n_negative, "\n")
```

```
##   Test statistic (number of - signs): 14
```

``` r
cat("  Sample size (non-zero):", n_nonzero, "\n")
```

```
##   Sample size (non-zero): 24
```

``` r
cat("  P-value:", round(p_value_sign, 4), "\n")
```

```
##   P-value: 0.8463
```

### Mathematical Derivation

Under $H_0$, the number of positive signs follows a binomial distribution:
$$S^+ \sim \text{Binomial}(n, p = 0.5)$$

For the one-sided test with $H_a: \text{median} < 120$, the p-value is:
$$p = P(S^- \geq 14 | H_0) = \sum_{k=14}^{24} \binom{24}{k} (0.5)^{24}$$

### Conclusion

With p-value = 0.8463 > 0.05, we **fail to reject $H_0$** at $\alpha = 0.05$. There is **insufficient evidence** to conclude that the median blood sugar reading is less than 120 mg/dL in this population.

\newpage

## Part b) Wilcoxon Signed-Rank Test

The Wilcoxon signed-rank test is more powerful than the sign test because it considers both the direction and magnitude of differences from the hypothesized median.


``` r
# Wilcoxon signed-rank test
wilcox_result <- wilcox.test(blood_sugar, mu = 120, alternative = "less", exact = FALSE)

cat("Wilcoxon Signed-Rank Test Results:\n")
```

```
## Wilcoxon Signed-Rank Test Results:
```

``` r
cat("  Test statistic (V):", wilcox_result$statistic, "\n")
```

```
##   Test statistic (V): 112.5
```

``` r
cat("  P-value:", round(wilcox_result$p.value, 4), "\n")
```

```
##   P-value: 0.1447
```

### Methodology

The Wilcoxon test procedure:
1. Calculate differences: $d_i = X_i - 120$
2. Rank absolute differences: $|d_i|$
3. Apply signs to ranks
4. Sum positive ranks: $W^+ = \sum_{d_i > 0} \text{rank}(|d_i|)$

Under $H_0$, the distribution of $W^+$ is approximately normal for $n \geq 10$:
$$W^+ \sim N\left(\frac{n(n+1)}{4}, \frac{n(n+1)(2n+1)}{24}\right)$$

### Conclusion

With p-value = 0.1447 > 0.05, we **fail to reject $H_0$**. There is **insufficient evidence** that the median blood sugar reading is less than 120 mg/dL.

### Comparison of Tests

Both tests lead to the same conclusion. The Wilcoxon test has a smaller p-value (0.1447) compared to the sign test (0.8463), demonstrating its greater statistical power by utilizing information about the magnitude of differences.

\newpage

# Problem 2: Brain Data Analysis (10 points)

## Problem Statement

We investigate whether humans have an excessive glia-neuron ratio for their brain mass compared to other primates, or if the human frontal cortex metabolic demands are simply a consequence of larger brain size.

## Data Loading and Preparation


``` r
# Load brain data
brain_data <- read_excel("data/Brain data.xlsx")

# Convert brain mass to numeric (handling potential formatting issues)
brain_data <- brain_data %>%
  mutate(`Brain mass (g)` = as.numeric(str_trim(`Brain mass (g)`)))

# Separate human and non-human data
nonhuman_data <- brain_data %>% 
  filter(Species != "Homo sapiens")

human_data <- brain_data %>% 
  filter(Species == "Homo sapiens")

cat("Non-human primates:", nrow(nonhuman_data), "species\n")
```

```
## Non-human primates: 16 species
```

``` r
cat("Humans:", nrow(human_data), "observation\n\n")
```

```
## Humans: 1 observation
```

``` r
# Display summary
kable(head(brain_data, 5), caption = "Brain Data Sample")
```



Table: Brain Data Sample

|Species            | Brain mass (g)| Ln Brain mass| Glia-neuron ratio|
|:------------------|--------------:|-------------:|-----------------:|
|Homo sapiens       |         1373.3|          7.22|              1.65|
|Pan troglodytes    |          336.2|          5.82|              1.20|
|Gorilla gorilla    |          509.2|          6.23|              1.21|
|Pongo pygmaeus     |          342.7|          5.84|              0.98|
|Hylobates muelleri |          101.8|          4.62|              1.22|

## Part a) Scatterplot and Regression Model

We fit a linear regression model using only non-human primate data with log-transformed brain mass as the predictor.

### Data Transformation


``` r
# Calculate log brain mass for non-human data
nonhuman_data <- nonhuman_data %>%
  mutate(log_brain_mass = log(`Brain mass (g)`))
```

### Regression Model

**Model:** 
$$\text{Glia-neuron ratio} = \beta_0 + \beta_1 \times \log(\text{Brain mass}) + \epsilon$$

where $\epsilon \sim N(0, \sigma^2)$


``` r
# Fit regression model
model_brain <- lm(`Glia-neuron ratio` ~ log_brain_mass, data = nonhuman_data)

# Model summary
summary(model_brain)
```

```
## 
## Call:
## lm(formula = `Glia-neuron ratio` ~ log_brain_mass, data = nonhuman_data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.23080 -0.12247 -0.03375  0.17943  0.26254 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     0.16624    0.16493   1.008 0.330588    
## log_brain_mass  0.17896    0.03723   4.807 0.000279 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.1751 on 14 degrees of freedom
## Multiple R-squared:  0.6227,	Adjusted R-squared:  0.5957 
## F-statistic: 23.11 on 1 and 14 DF,  p-value: 0.000279
```

``` r
# Extract coefficients
coef_summary <- tidy(model_brain)
kable(coef_summary, digits = 4, caption = "Regression Coefficients")
```



Table: Regression Coefficients

|term           | estimate| std.error| statistic| p.value|
|:--------------|--------:|---------:|---------:|-------:|
|(Intercept)    |   0.1662|    0.1649|    1.0079|  0.3306|
|log_brain_mass |   0.1790|    0.0372|    4.8068|  0.0003|

``` r
# Store coefficients for reporting
beta_0 <- coef(model_brain)[1]
beta_1 <- coef(model_brain)[2]
r_squared <- summary(model_brain)$r.squared
```

### Fitted Regression Equation

$$\hat{Y} = 0.1662 + 0.179 \times \log(\text{Brain mass})$$

where:
- $\beta_0 = 0.1662$ (intercept)
- $\beta_1 = 0.179$ (slope)
- $R^2 = 0.6227$ (62.27% of variation explained)
- p-value < 0.001 (highly significant)

### Interpretation

For each unit increase in log(brain mass), the glia-neuron ratio increases by 0.179 on average. This positive relationship is statistically significant and explains 62.3% of the variation in glia-neuron ratio among non-human primates.

### Scatterplot


``` r
# Create scatterplot with regression line
ggplot(nonhuman_data, aes(x = log_brain_mass, y = `Glia-neuron ratio`)) +
  geom_point(size = 3, color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink", alpha = 0.3) +
  labs(
    title = "Glia-Neuron Ratio vs Log Brain Mass",
    subtitle = "Non-Human Primates Only",
    x = "Log(Brain Mass) [g]",
    y = "Glia-Neuron Ratio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12)
  )
```

![Glia-Neuron Ratio vs Log Brain Mass for Non-Human Primates](HW4_BowenXia_bx2232_files/figure-latex/problem2-plot-1.pdf) 

\newpage

## Part b) Prediction for Humans

Using the non-human primate relationship, we predict the glia-neuron ratio for humans given their brain mass.


``` r
# Get human brain mass
human_brain_mass <- human_data$`Brain mass (g)`
human_log_brain_mass <- log(human_brain_mass)

# Predict glia-neuron ratio for humans
predicted_human <- predict(model_brain, 
                          newdata = data.frame(log_brain_mass = human_log_brain_mass))

actual_human <- human_data$`Glia-neuron ratio`

cat("Human brain mass:", round(human_brain_mass, 1), "g\n")
```

```
## Human brain mass: 1373.3 g
```

``` r
cat("Log(brain mass):", round(human_log_brain_mass, 4), "\n\n")
```

```
## Log(brain mass): 7.225
```

``` r
cat("Predicted glia-neuron ratio:", round(predicted_human, 4), "\n")
```

```
## Predicted glia-neuron ratio: 1.4592
```

``` r
cat("Actual human glia-neuron ratio:", actual_human, "\n")
```

```
## Actual human glia-neuron ratio: 1.65
```

``` r
cat("Difference (Actual - Predicted):", round(actual_human - predicted_human, 4), "\n")
```

```
## Difference (Actual - Predicted): 0.1908
```

### Calculation

$$\hat{Y}_{\text{human}} = 0.1662 + 0.179 \times 7.225 = 1.4592$$

The actual human value (1.65) is higher than predicted (1.4592), with a difference of 0.1908.

\newpage

## Part c) 95% Prediction Interval

A prediction interval accounts for both the uncertainty in the regression line and the variability of individual observations.


``` r
# Calculate 95% prediction interval
pred_interval <- predict(model_brain,
                        newdata = data.frame(log_brain_mass = human_log_brain_mass),
                        interval = "prediction",
                        level = 0.95)

cat("95% Prediction Interval for Humans:\n")
```

```
## 95% Prediction Interval for Humans:
```

``` r
cat("  Lower bound:", round(pred_interval[2], 4), "\n")
```

```
##   Lower bound: 1.0059
```

``` r
cat("  Predicted value:", round(pred_interval[1], 4), "\n")
```

```
##   Predicted value: 1.4592
```

``` r
cat("  Upper bound:", round(pred_interval[3], 4), "\n")
```

```
##   Upper bound: 1.9125
```

``` r
cat("  Actual human value:", actual_human, "\n\n")
```

```
##   Actual human value: 1.65
```

``` r
# Check if human value is within the interval
in_interval <- actual_human >= pred_interval[2] & actual_human <= pred_interval[3]

if (in_interval) {
  cat("The human glia-neuron ratio FALLS WITHIN the 95% prediction interval.\n")
} else {
  cat("The human glia-neuron ratio EXCEEDS the 95% prediction interval.\n")
}
```

```
## The human glia-neuron ratio FALLS WITHIN the 95% prediction interval.
```

### Mathematical Formula

The 95% prediction interval is:
$$\hat{Y} \pm t_{n-2, 0.025} \times SE_{\text{pred}}$$

where
$$SE_{\text{pred}} = s \sqrt{1 + \frac{1}{n} + \frac{(X_0 - \bar{X})^2}{\sum(X_i - \bar{X})^2}}$$

and $s = \sqrt{MSE}$ is the residual standard error.

### Conclusion

The human glia-neuron ratio (1.65) falls within the 95% prediction interval [1.0059, 1.9125]. 

This suggests that humans do NOT have a statistically excessive glia-neuron ratio for their brain mass compared to other primates, when accounting for prediction uncertainty.

\newpage

## Part d) Cautions About Extrapolation

Several important considerations when using non-human primate data to make predictions about humans:

### 1. Extrapolation Beyond Data Range


``` r
# Check data range
nonhuman_range <- range(nonhuman_data$log_brain_mass)
cat("Range of log(brain mass) in non-human data:", 
    round(nonhuman_range[1], 3), "to", round(nonhuman_range[2], 3), "\n")
```

```
## Range of log(brain mass) in non-human data: 2.303 to 6.233
```

``` r
cat("Human log(brain mass):", round(human_log_brain_mass, 3), "\n\n")
```

```
## Human log(brain mass): 7.225
```

``` r
if (human_log_brain_mass > nonhuman_range[2]) {
  cat("⚠️ WARNING: Human brain mass is BEYOND the range of non-human data!\n")
  cat("This is EXTRAPOLATION, which is less reliable than interpolation.\n")
}
```

```
## ⚠️ WARNING: Human brain mass is BEYOND the range of non-human data!
## This is EXTRAPOLATION, which is less reliable than interpolation.
```

**Issue:** The human brain mass (log scale: 7.225) exceeds the maximum in the non-human data (log scale: 6.233). Predictions outside the observed data range are inherently less reliable because we assume the linear relationship continues beyond where we have data.

### 2. Species-Specific Differences

Humans possess unique evolutionary adaptations:
- Larger and more complex frontal cortex
- Different neuronal density and organization
- Unique cognitive capabilities suggesting distinct brain architecture
- Different metabolic regulation patterns

### 3. Model Assumptions

The analysis assumes:
- **Linearity:** The relationship remains linear on the log scale across all primates
- **Constant variance:** Variability is similar across the range (homoscedasticity)
- **Independence:** Each species is an independent observation
- **Normality:** Residuals are normally distributed

These assumptions may not hold when extending to humans.

### 4. Sample Size Limitations

With only 16 non-human primate species, the model has limited precision. The width of the prediction interval reflects this uncertainty, but the extrapolation adds additional uncertainty not captured by the interval.

### 5. Biological Mechanisms

The relationship between brain size and glia-neuron ratio may be governed by different biological mechanisms in humans versus other primates, particularly given:
- Longer lifespan
- Extended period of brain development
- Different energy metabolism
- Unique selective pressures during evolution

### Recommendation

While this analysis provides useful context, conclusions about human exceptionality should be made cautiously. The combination of extrapolation, species differences, and limited sample size suggests that **humans may differ from other primates in ways not captured by this simple model**. Additional data on great apes with larger brains, or mechanistic studies of glia-neuron relationships, would strengthen inferences about humans.

\newpage

# Problem 3: Heart Disease Cost Analysis (20 points)

## Problem Statement

An investigator wants to determine if there is an association between total cost (dollars) of patients diagnosed with heart disease and the number of emergency room (ER) visits. The model may need adjustment for age, gender, number of complications, and duration of treatment.

## Data Loading


``` r
# Load heart disease data
heart_data <- read_csv("data/HeartDisease.csv")

cat("Data dimensions:", nrow(heart_data), "observations,", ncol(heart_data), "variables\n\n")
```

```
## Data dimensions: 788 observations, 10 variables
```

``` r
cat("Variable names:\n")
```

```
## Variable names:
```

``` r
cat(paste(names(heart_data), collapse = ", "), "\n")
```

```
## id, totalcost, age, gender, interventions, drugs, ERvisits, complications, comorbidities, duration
```

## Part a) Descriptive Statistics

### Continuous Variables


``` r
# Select continuous variables
continuous_vars <- c("totalcost", "age", "interventions", "drugs", 
                    "ERvisits", "complications", "comorbidities", "duration")

# Create summary statistics
desc_stats <- heart_data %>%
  select(all_of(continuous_vars)) %>%
  summarise(across(everything(), 
                  list(Mean = ~mean(., na.rm = TRUE),
                       SD = ~sd(., na.rm = TRUE),
                       Median = ~median(., na.rm = TRUE),
                       Q1 = ~quantile(., 0.25, na.rm = TRUE),
                       Q3 = ~quantile(., 0.75, na.rm = TRUE),
                       Min = ~min(., na.rm = TRUE),
                       Max = ~max(., na.rm = TRUE)),
                  .names = "{.col}_{.fn}"))

# Reshape for better display
desc_long <- desc_stats %>%
  pivot_longer(everything(), names_to = "stat", values_to = "value") %>%
  separate(stat, into = c("Variable", "Statistic"), sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = Statistic, values_from = value)

kable(desc_long, digits = 2, caption = "Descriptive Statistics for Continuous Variables")
```



Table: Descriptive Statistics for Continuous Variables

|Variable      |    Mean|      SD| Median|     Q1|      Q3| Min|     Max|
|:-------------|-------:|-------:|------:|------:|-------:|---:|-------:|
|totalcost     | 2799.96| 6690.26|  507.2| 161.12| 1905.45|   0| 52664.9|
|age           |   58.72|    6.75|   60.0|  55.00|   64.00|  24|    70.0|
|interventions |    4.71|    5.59|    3.0|   1.00|    6.00|   0|    47.0|
|drugs         |    0.45|    1.06|    0.0|   0.00|    0.00|   0|     9.0|
|ERvisits      |    3.43|    2.64|    3.0|   2.00|    5.00|   0|    20.0|
|complications |    0.06|    0.25|    0.0|   0.00|    0.00|   0|     3.0|
|comorbidities |    3.77|    5.95|    1.0|   0.00|    5.00|   0|    60.0|
|duration      |  164.03|  120.92|  165.5|  41.75|  281.00|   0|   372.0|

### Categorical Variables


``` r
# Gender distribution
gender_table <- table(heart_data$gender)
gender_prop <- prop.table(gender_table)

cat("Gender Distribution:\n")
```

```
## Gender Distribution:
```

``` r
cat("  Male (0):", gender_table[1], sprintf("(%.1f%%)\n", gender_prop[1]*100))
```

```
##   Male (0): 608 (77.2%)
```

``` r
cat("  Female (1):", gender_table[2], sprintf("(%.1f%%)\n", gender_prop[2]*100))
```

```
##   Female (1): 180 (22.8%)
```

### Key Observations

- **Total cost** is highly variable (range: $0 to $52,664.9), suggesting right-skewed distribution
- **Mean cost** ($2,799.96) >> **Median cost** ($507.2) confirms right skew
- Most patients (77%) are male
- Complications are rare (mean = 0.06)

\newpage

## Part b) Distribution and Transformation

### Investigate Distribution


``` r
# Check for zeros
n_zeros <- sum(heart_data$totalcost == 0)
cat("Number of zero values in totalcost:", n_zeros, "\n")
```

```
## Number of zero values in totalcost: 3
```

``` r
cat("These will be excluded for log transformation.\n\n")
```

```
## These will be excluded for log transformation.
```

``` r
# Filter out zeros for transformation analysis
heart_pos <- heart_data %>% filter(totalcost > 0)

# Create transformations
heart_trans <- heart_pos %>%
  mutate(
    log_cost = log(totalcost),
    sqrt_cost = sqrt(totalcost)
  )

# Normality tests
shapiro_orig <- shapiro.test(sample(heart_pos$totalcost, min(5000, nrow(heart_pos))))
shapiro_log <- shapiro.test(sample(heart_trans$log_cost, min(5000, nrow(heart_trans))))
shapiro_sqrt <- shapiro.test(sample(heart_trans$sqrt_cost, min(5000, nrow(heart_trans))))

cat("Shapiro-Wilk Tests for Normality:\n")
```

```
## Shapiro-Wilk Tests for Normality:
```

``` r
cat("  Original: W =", round(shapiro_orig$statistic, 4), ", p-value =", 
    format.pval(shapiro_orig$p.value, digits = 3), "\n")
```

```
##   Original: W = 0.4406 , p-value = <2e-16
```

``` r
cat("  Log: W =", round(shapiro_log$statistic, 4), ", p-value =", 
    format.pval(shapiro_log$p.value, digits = 3), "\n")
```

```
##   Log: W = 0.9952 , p-value = 0.0149
```

``` r
cat("  Sqrt: W =", round(shapiro_sqrt$statistic, 4), ", p-value =", 
    format.pval(shapiro_sqrt$p.value, digits = 3), "\n\n")
```

```
##   Sqrt: W = 0.7303 , p-value = <2e-16
```

``` r
# Calculate skewness
library(e1071)
skew_orig <- skewness(heart_pos$totalcost)
skew_log <- skewness(heart_trans$log_cost)
skew_sqrt <- skewness(heart_trans$sqrt_cost)

cat("Skewness:\n")
```

```
## Skewness:
```

``` r
cat("  Original:", round(skew_orig, 3), "\n")
```

```
##   Original: 4.204
```

``` r
cat("  Log-transformed:", round(skew_log, 3), "\n")
```

```
##   Log-transformed: 0.087
```

``` r
cat("  Square-root transformed:", round(skew_sqrt, 3), "\n\n")
```

```
##   Square-root transformed: 2.313
```

``` r
# Create plots
p1 <- ggplot(heart_pos, aes(x = totalcost)) +
  geom_histogram(bins = 40, fill = "lightblue", color = "black") +
  labs(title = "Original Scale", x = "Total Cost", y = "Frequency") +
  theme_minimal()

p2 <- ggplot(heart_trans, aes(sample = totalcost)) +
  stat_qq() + stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot (Original)") +
  theme_minimal()

p3 <- ggplot(heart_trans, aes(x = log_cost)) +
  geom_histogram(bins = 40, fill = "lightgreen", color = "black") +
  labs(title = "Log Transform", x = "Log(Total Cost)", y = "Frequency") +
  theme_minimal()

p4 <- ggplot(heart_trans, aes(sample = log_cost)) +
  stat_qq() + stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot (Log)") +
  theme_minimal()

p5 <- ggplot(heart_trans, aes(x = sqrt_cost)) +
  geom_histogram(bins = 40, fill = "lightyellow", color = "black") +
  labs(title = "Square Root Transform", x = "√(Total Cost)", y = "Frequency") +
  theme_minimal()

p6 <- ggplot(heart_trans, aes(sample = sqrt_cost)) +
  stat_qq() + stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot (Sqrt)") +
  theme_minimal()

# Arrange plots
(p1 | p2) / (p3 | p4) / (p5 | p6)
```

![Distribution of Total Cost: Original and Transformed](HW4_BowenXia_bx2232_files/figure-latex/problem3-distribution-1.pdf) 

### Recommendation

Based on the diagnostic tests:

- **Original scale**: Severely right-skewed (skewness = 4.2)
- **Log transformation**: Nearly symmetric (skewness = 0.09) ✓
- **Square root**: Still right-skewed (skewness = 2.31)

**Decision: Use LOG TRANSFORMATION**

The log transformation reduces skewness dramatically and produces a distribution much closer to normal. This is standard practice for cost data, which typically follows a log-normal distribution.


``` r
# Create working dataset with log transformation
heart_analysis <- heart_data %>%
  filter(totalcost > 0) %>%  # Remove zeros for log transformation
  mutate(log_totalcost = log(totalcost))

cat("After removing", n_zeros, "zero values, n =", nrow(heart_analysis), "\n")
```

```
## After removing 3 zero values, n = 785
```

\newpage

## Part c) Create comp_bin Variable


``` r
# Create binary complications variable
heart_analysis <- heart_analysis %>%
  mutate(comp_bin = ifelse(complications == 0, 0, 1))

# Summary
cat("Complications Variable (original):\n")
```

```
## Complications Variable (original):
```

``` r
print(table(heart_analysis$complications))
```

```
## 
##   0   1   3 
## 742  42   1
```

``` r
cat("\n\nComp_bin Variable (binary):\n")
```

```
## 
## 
## Comp_bin Variable (binary):
```

``` r
print(table(heart_analysis$comp_bin))
```

```
## 
##   0   1 
## 742  43
```

``` r
cat("\n\nInterpretation:\n")
```

```
## 
## 
## Interpretation:
```

``` r
cat("  comp_bin = 0: No complications (", sum(heart_analysis$comp_bin == 0), "patients)\n")
```

```
##   comp_bin = 0: No complications ( 742 patients)
```

``` r
cat("  comp_bin = 1: ≥1 complication (", sum(heart_analysis$comp_bin == 1), "patients)\n")
```

```
##   comp_bin = 1: ≥1 complication ( 43 patients)
```

\newpage

## Part d) Simple Linear Regression (SLR)

### Model Specification

**Model:**
$$\log(\text{totalcost}) = \beta_0 + \beta_1 \times \text{ERvisits} + \epsilon$$

where $\epsilon \sim N(0, \sigma^2)$


``` r
# Fit simple linear regression
slr_model <- lm(log_totalcost ~ ERvisits, data = heart_analysis)

# Model summary
summary(slr_model)
```

```
## 
## Call:
## lm(formula = log_totalcost ~ ERvisits, data = heart_analysis)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.2013 -1.1265  0.0191  1.2668  4.2797 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.53771    0.10362   53.44   <2e-16 ***
## ERvisits     0.22672    0.02397    9.46   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.772 on 783 degrees of freedom
## Multiple R-squared:  0.1026,	Adjusted R-squared:  0.1014 
## F-statistic:  89.5 on 1 and 783 DF,  p-value: < 2.2e-16
```

``` r
# Store results
slr_summary <- tidy(slr_model)
slr_coef_er <- slr_summary$estimate[2]
slr_pval <- slr_summary$p.value[2]
slr_r2 <- summary(slr_model)$r.squared
```

### Results Table


``` r
kable(tidy(slr_model), digits = 4, caption = "Simple Linear Regression Results")
```



Table: Simple Linear Regression Results

|term        | estimate| std.error| statistic| p.value|
|:-----------|--------:|---------:|---------:|-------:|
|(Intercept) |   5.5377|    0.1036|   53.4442|       0|
|ERvisits    |   0.2267|    0.0240|    9.4603|       0|

### Fitted Equation

$$\widehat{\log(\text{totalcost})} = 5.5377 + 0.2267 \times \text{ERvisits}$$

### Interpretation

**Slope ($\beta_1 = 0.2267$):**

On the log scale: For each additional ER visit, log(total cost) increases by 0.2267 units.

On the original scale: For each additional ER visit, total cost increases by a multiplicative factor of $e^{0.2267} = 1.2545$, corresponding to a **25.45% increase** in cost.

**Statistical Significance:** 
- P-value < 0.001 (highly significant)
- 95% CI: [0.1797, 0.2738]

**Model Fit:**
- $R^2 = 0.1026$ (10.26% of variance explained)

### Scatterplot


``` r
ggplot(heart_analysis, aes(x = ERvisits, y = log_totalcost)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink") +
  labs(
    title = "Log(Total Cost) vs ER Visits",
    x = "Number of ER Visits",
    y = "Log(Total Cost)",
    subtitle = sprintf("R² = %.4f, β₁ = %.4f, p < 0.001", slr_r2, slr_coef_er)
  ) +
  theme_minimal()
```

![Simple Linear Regression: Log(Total Cost) vs ER Visits](HW4_BowenXia_bx2232_files/figure-latex/problem3-slr-plot-1.pdf) 

**Conclusion:** There is a **significant positive association** between ER visits and total cost. Each additional ER visit is associated with a 25.45% increase in cost.

\newpage

## Part e) Multiple Linear Regression with comp_bin

### Part e.I) Test for Interaction

We test whether the effect of ER visits on cost differs by complication status.

**Model with Interaction:**
$$\log(\text{totalcost}) = \beta_0 + \beta_1 \times \text{ERvisits} + \beta_2 \times \text{comp\_bin} + \beta_3 \times (\text{ERvisits} \times \text{comp\_bin}) + \epsilon$$


``` r
# Fit model with interaction
mlr_interaction <- lm(log_totalcost ~ ERvisits * comp_bin, data = heart_analysis)

# Summary
summary(mlr_interaction)
```

```
## 
## Call:
## lm(formula = log_totalcost ~ ERvisits * comp_bin, data = heart_analysis)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.0852 -1.0802 -0.0078  1.1898  4.3803 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        5.49899    0.10349  53.138  < 2e-16 ***
## ERvisits           0.21125    0.02453   8.610  < 2e-16 ***
## comp_bin           2.17969    0.54604   3.992 7.17e-05 ***
## ERvisits:comp_bin -0.09927    0.09483  -1.047    0.296    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.732 on 781 degrees of freedom
## Multiple R-squared:  0.1449,	Adjusted R-squared:  0.1417 
## F-statistic: 44.13 on 3 and 781 DF,  p-value: < 2.2e-16
```

``` r
# Extract interaction p-value
int_pval <- tidy(mlr_interaction) %>% 
  filter(term == "ERvisits:comp_bin") %>% 
  pull(p.value)

kable(tidy(mlr_interaction), digits = 4, caption = "MLR with Interaction")
```



Table: MLR with Interaction

|term              | estimate| std.error| statistic| p.value|
|:-----------------|--------:|---------:|---------:|-------:|
|(Intercept)       |   5.4990|    0.1035|   53.1380|  0.0000|
|ERvisits          |   0.2112|    0.0245|    8.6103|  0.0000|
|comp_bin          |   2.1797|    0.5460|    3.9918|  0.0001|
|ERvisits:comp_bin |  -0.0993|    0.0948|   -1.0467|  0.2955|

**Interpretation of Coefficients:**

- $\beta_1$ (`ERvisits`): Effect of ER visits when comp_bin = 0 (no complications)
- $\beta_2$ (`comp_bin`): Difference in intercept for patients with complications
- $\beta_3$ (`ERvisits:comp_bin`): **Additional** effect of ER visits for patients with complications

**Test for Interaction:**
$$H_0: \beta_3 = 0 \text{ vs. } H_a: \beta_3 \neq 0$$

**Results:**
- Interaction coefficient: -0.0993
- P-value: 0.2955

**Conclusion:** With p-value = 0.2955 < 0.05, we fail to reject $H_0$. There is NO significant evidence that the effect of ER visits on cost differs by complication status. The **parallel slopes model** is appropriate.

\newpage

### Part e.II) Test for Confounding

A variable is a confounder if:
1. It's associated with both the predictor and outcome
2. Adjusting for it changes the coefficient of the primary predictor by >10%


``` r
# Fit model without interaction
mlr_no_interaction <- lm(log_totalcost ~ ERvisits + comp_bin, data = heart_analysis)

# Compare coefficients
coef_slr <- coef(slr_model)["ERvisits"]
coef_mlr <- coef(mlr_no_interaction)["ERvisits"]
pct_change <- abs((coef_mlr - coef_slr) / coef_slr) * 100

cat("Confounding Assessment:\n")
```

```
## Confounding Assessment:
```

``` r
cat("  ERvisits coefficient in SLR:", round(coef_slr, 4), "\n")
```

```
##   ERvisits coefficient in SLR: 0.2267
```

``` r
cat("  ERvisits coefficient in MLR (+ comp_bin):", round(coef_mlr, 4), "\n")
```

```
##   ERvisits coefficient in MLR (+ comp_bin): 0.2046
```

``` r
cat("  Absolute change:", round(abs(coef_mlr - coef_slr), 4), "\n")
```

```
##   Absolute change: 0.0221
```

``` r
cat("  Percent change:", round(pct_change, 2), "%\n\n")
```

```
##   Percent change: 9.76 %
```

``` r
# Summary of MLR
summary(mlr_no_interaction)
```

```
## 
## Call:
## lm(formula = log_totalcost ~ ERvisits + comp_bin, data = heart_analysis)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.0741 -1.0737 -0.0181  1.1810  4.3848 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   5.5211     0.1013  54.495  < 2e-16 ***
## ERvisits      0.2046     0.0237   8.633  < 2e-16 ***
## comp_bin      1.6859     0.2749   6.132 1.38e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.732 on 782 degrees of freedom
## Multiple R-squared:  0.1437,	Adjusted R-squared:  0.1416 
## F-statistic: 65.64 on 2 and 782 DF,  p-value: < 2.2e-16
```

``` r
kable(tidy(mlr_no_interaction), digits = 4, caption = "MLR without Interaction")
```



Table: MLR without Interaction

|term        | estimate| std.error| statistic| p.value|
|:-----------|--------:|---------:|---------:|-------:|
|(Intercept) |   5.5211|    0.1013|   54.4954|       0|
|ERvisits    |   0.2046|    0.0237|    8.6329|       0|
|comp_bin    |   1.6859|    0.2749|    6.1317|       0|

**Interpretation:**

The ERvisits coefficient changes from 0.2267 (SLR) to 0.2046 (MLR), a 9.76% change.

**Conclusion:** Since the coefficient changes by less than 10%, comp_bin is NOT a confounder of the relationship between ER visits and total cost.



### Part e.III) Should comp_bin be Included?


``` r
comp_pval <- tidy(mlr_no_interaction) %>% 
  filter(term == "comp_bin") %>% 
  pull(p.value)

cat("comp_bin in MLR:\n")
```

```
## comp_bin in MLR:
```

``` r
cat("  Coefficient:", round(coef(mlr_no_interaction)["comp_bin"], 4), "\n")
```

```
##   Coefficient: 1.6859
```

``` r
cat("  P-value:", format.pval(comp_pval, digits = 4), "\n\n")
```

```
##   P-value: 1.379e-09
```

``` r
cat("Decision Criteria:\n")
```

```
## Decision Criteria:
```

``` r
cat("  1. Statistical significance (p < 0.05):", comp_pval < 0.05, "\n")
```

```
##   1. Statistical significance (p < 0.05): TRUE
```

``` r
cat("  2. Confounding (>10% change):", pct_change > 10, "\n\n")
```

```
##   2. Confounding (>10% change): FALSE
```

**Decision: INCLUDE comp_bin in the model**

**Reasoning:**
- comp_bin is **statistically significant** (p < 0.05)
- Clinically meaningful: complications substantially impact costs
- Omitting comp_bin would produce biased estimates of the ER visits effect

\newpage

## Part f) Full Multiple Linear Regression

### Part f.I) Full MLR with All Covariates

Based on Part e analysis, we include comp_bin along with other covariates.

**Model:**
$$\log(\text{totalcost}) = \beta_0 + \beta_1 \times \text{ERvisits} + \beta_2 \times \text{comp\_bin} + \beta_3 \times \text{age} + \beta_4 \times \text{gender} + \beta_5 \times \text{duration} + \epsilon$$


``` r
# Fit full model
full_mlr <- lm(log_totalcost ~ ERvisits + comp_bin + age + gender + duration, 
               data = heart_analysis)

# Model summary
summary(full_mlr)
```

```
## 
## Call:
## lm(formula = log_totalcost ~ ERvisits + comp_bin + age + gender + 
##     duration, data = heart_analysis)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.0823 -1.0555 -0.1352  0.9533  4.3462 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.0449619  0.5063454  11.938  < 2e-16 ***
## ERvisits     0.1757486  0.0223189   7.874 1.15e-14 ***
## comp_bin     1.4921110  0.2554883   5.840 7.65e-09 ***
## age         -0.0221376  0.0086023  -2.573   0.0103 *  
## gender      -0.1176181  0.1379809  -0.852   0.3942    
## duration     0.0055406  0.0004848  11.428  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.605 on 779 degrees of freedom
## Multiple R-squared:  0.268,	Adjusted R-squared:  0.2633 
## F-statistic: 57.03 on 5 and 779 DF,  p-value: < 2.2e-16
```

``` r
# Coefficient table
kable(tidy(full_mlr), digits = 4, caption = "Full Multiple Linear Regression Results")
```



Table: Full Multiple Linear Regression Results

|term        | estimate| std.error| statistic| p.value|
|:-----------|--------:|---------:|---------:|-------:|
|(Intercept) |   6.0450|    0.5063|   11.9384|  0.0000|
|ERvisits    |   0.1757|    0.0223|    7.8744|  0.0000|
|comp_bin    |   1.4921|    0.2555|    5.8402|  0.0000|
|age         |  -0.0221|    0.0086|   -2.5735|  0.0103|
|gender      |  -0.1176|    0.1380|   -0.8524|  0.3942|
|duration    |   0.0055|    0.0005|   11.4281|  0.0000|

### Fitted Regression Equation

$$\begin{aligned}
\widehat{\log(\text{totalcost})} = &\ 6.045 \\
&+ 0.1757 \times \text{ERvisits} \\
&+ 1.4921 \times \text{comp\_bin} \\
&-0.0221 \times \text{age} \\
&-0.1176 \times \text{gender} \\
&+ 0.0055 \times \text{duration}
\end{aligned}$$

### Interpretation of Each Coefficient


``` r
# Extract coefficients and p-values
coefs <- tidy(full_mlr)

# Function to interpret log-scale coefficient
interpret_coef <- function(beta) {
  pct <- (exp(beta) - 1) * 100
  return(round(pct, 2))
}

# Create interpretation text
cat("Coefficient Interpretations:\n\n")
```

```
## Coefficient Interpretations:
```

``` r
cat("1. ERvisits (β₁ =", round(coefs$estimate[2], 4), ", p =", 
    format.pval(coefs$p.value[2], digits = 3), "):\n")
```

```
## 1. ERvisits (β₁ = 0.1757 , p = 1.15e-14 ):
```

``` r
cat("   Each additional ER visit increases total cost by", 
    interpret_coef(coefs$estimate[2]), "%\n")
```

```
##    Each additional ER visit increases total cost by 19.21 %
```

``` r
cat("  ", ifelse(coefs$p.value[2] < 0.05, "✓ SIGNIFICANT", "✗ Not significant"), "\n\n")
```

```
##    ✓ SIGNIFICANT
```

``` r
cat("2. comp_bin (β₂ =", round(coefs$estimate[3], 4), ", p =", 
    format.pval(coefs$p.value[3], digits = 3), "):\n")
```

```
## 2. comp_bin (β₂ = 1.4921 , p = 7.65e-09 ):
```

``` r
cat("   Having complications increases total cost by", 
    interpret_coef(coefs$estimate[3]), "%\n")
```

```
##    Having complications increases total cost by 344.65 %
```

``` r
cat("  ", ifelse(coefs$p.value[3] < 0.05, "✓ SIGNIFICANT", "✗ Not significant"), "\n\n")
```

```
##    ✓ SIGNIFICANT
```

``` r
cat("3. age (β₃ =", round(coefs$estimate[4], 4), ", p =", 
    format.pval(coefs$p.value[4], digits = 3), "):\n")
```

```
## 3. age (β₃ = -0.0221 , p = 0.0103 ):
```

``` r
cat("   Each additional year of age changes total cost by", 
    interpret_coef(coefs$estimate[4]), "%\n")
```

```
##    Each additional year of age changes total cost by -2.19 %
```

``` r
cat("  ", ifelse(coefs$p.value[4] < 0.05, "✓ SIGNIFICANT", "✗ Not significant"), "\n\n")
```

```
##    ✓ SIGNIFICANT
```

``` r
cat("4. gender (β₄ =", round(coefs$estimate[5], 4), ", p =", 
    format.pval(coefs$p.value[5], digits = 3), "):\n")
```

```
## 4. gender (β₄ = -0.1176 , p = 0.394 ):
```

``` r
cat("   Gender difference in total cost:", 
    interpret_coef(coefs$estimate[5]), "%\n")
```

```
##    Gender difference in total cost: -11.1 %
```

``` r
cat("  ", ifelse(coefs$p.value[5] < 0.05, "✓ SIGNIFICANT", "✗ Not significant"), "\n\n")
```

```
##    ✗ Not significant
```

``` r
cat("5. duration (β₅ =", round(coefs$estimate[6], 4), ", p =", 
    format.pval(coefs$p.value[6], digits = 3), "):\n")
```

```
## 5. duration (β₅ = 0.0055 , p = <2e-16 ):
```

``` r
cat("   Each additional day of treatment increases total cost by", 
    interpret_coef(coefs$estimate[6]), "%\n")
```

```
##    Each additional day of treatment increases total cost by 0.56 %
```

``` r
cat("  ", ifelse(coefs$p.value[6] < 0.05, "✓ SIGNIFICANT", "✗ Not significant"), "\n\n")
```

```
##    ✓ SIGNIFICANT
```

**Significant Variables (α = 0.05):**

``` r
sig_vars <- coefs %>% 
  filter(p.value < 0.05, term != "(Intercept)") %>% 
  pull(term)

cat(paste(sig_vars, collapse = ", "))
```

```
## ERvisits, comp_bin, age, duration
```

\newpage

### Part f.II) Model Comparison

We compare the SLR (ERvisits only) to the full MLR using a nested model F-test.

**Hypotheses:**
$$H_0: \beta_2 = \beta_3 = \beta_4 = \beta_5 = 0 \text{ vs. } H_a: \text{at least one } \beta_j \neq 0$$


``` r
# ANOVA for nested models
anova_result <- anova(slr_model, full_mlr)

kable(tidy(anova_result), digits = 4, caption = "ANOVA for Nested Model Comparison")
```



Table: ANOVA for Nested Model Comparison

|term                                                          | df.residual|      rss| df|    sumsq| statistic| p.value|
|:-------------------------------------------------------------|-----------:|--------:|--:|--------:|---------:|-------:|
|log_totalcost ~ ERvisits                                      |         783| 2459.849| NA|       NA|        NA|      NA|
|log_totalcost ~ ERvisits + comp_bin + age + gender + duration |         779| 2006.552|  4| 453.2972|   43.9957|       0|

``` r
# Calculate additional statistics
r2_slr <- summary(slr_model)$r.squared
r2_full <- summary(full_mlr)$r.squared
adj_r2_slr <- summary(slr_model)$adj.r.squared
adj_r2_full <- summary(full_mlr)$adj.r.squared
f_stat <- anova_result$F[2]
f_pval <- anova_result$`Pr(>F)`[2]

cat("\nModel Comparison Summary:\n")
```

```
## 
## Model Comparison Summary:
```

``` r
cat("─────────────────────────────────────────\n")
```

```
## ─────────────────────────────────────────
```

``` r
cat("SLR Model:\n")
```

```
## SLR Model:
```

``` r
cat("  R²:", round(r2_slr, 4), "\n")
```

```
##   R²: 0.1026
```

``` r
cat("  Adjusted R²:", round(adj_r2_slr, 4), "\n")
```

```
##   Adjusted R²: 0.1014
```

``` r
cat("  Predictors: ERvisits only\n\n")
```

```
##   Predictors: ERvisits only
```

``` r
cat("Full MLR Model:\n")
```

```
## Full MLR Model:
```

``` r
cat("  R²:", round(r2_full, 4), "\n")
```

```
##   R²: 0.268
```

``` r
cat("  Adjusted R²:", round(adj_r2_full, 4), "\n")
```

```
##   Adjusted R²: 0.2633
```

``` r
cat("  Predictors: ERvisits + comp_bin + age + gender + duration\n\n")
```

```
##   Predictors: ERvisits + comp_bin + age + gender + duration
```

``` r
cat("Improvement:\n")
```

```
## Improvement:
```

``` r
cat("  ΔR²:", round(r2_full - r2_slr, 4), 
    sprintf("(%.1f%% → %.1f%%)\n", r2_slr*100, r2_full*100))
```

```
##   ΔR²: 0.1654 (10.3% → 26.8%)
```

``` r
cat("  ΔAdj R²:", round(adj_r2_full - adj_r2_slr, 4), "\n\n")
```

```
##   ΔAdj R²: 0.1618
```

``` r
cat("F-test:\n")
```

```
## F-test:
```

``` r
cat("  F-statistic:", round(f_stat, 4), "\n")
```

```
##   F-statistic: 43.9957
```

``` r
cat("  P-value:", format.pval(f_pval, digits = 4), "\n")
```

```
##   P-value: < 2.2e-16
```

### Mathematical Formula for F-test

The F-statistic for comparing nested models is:
$$F = \frac{(RSS_{\text{reduced}} - RSS_{\text{full}}) / (p_{\text{full}} - p_{\text{reduced}})}{RSS_{\text{full}} / (n - p_{\text{full}})}$$

Under $H_0$, this follows an $F$-distribution with degrees of freedom $(p_{\text{full}} - p_{\text{reduced}}, n - p_{\text{full}})$.

### Recommendation

**Decision: Use the FULL MLR model**

**Reasoning:**

1. **Statistical Evidence:** The F-test is highly significant (p < 0.001), indicating that the additional predictors significantly improve model fit.

2. **Explained Variance:** The full MLR explains 26.8% of variance compared to only 10.3% for the SLR—a 16.5 percentage point improvement.

3. **Confounder Control:** The MLR adjusts for important confounders (complications, age, duration), providing a more accurate estimate of the ER visits effect.

4. **Clinical Relevance:** All additional predictors except gender are statistically significant and clinically meaningful.

5. **Research Objective:** The MLR better addresses the investigator's goal by isolating the effect of ER visits while controlling for other factors that affect costs.

6. **Effect Size:** The ER visits effect is reduced from 25.4% (SLR) to 19.2% (MLR), suggesting confounding by omitted variables in the simple model.

### Diagnostic Plots


``` r
# Create diagnostic plots
par(mfrow = c(2, 2))
plot(full_mlr, which = 1:4)
```

![Diagnostic Plots for Full MLR Model](HW4_BowenXia_bx2232_files/figure-latex/problem3-diagnostics-1.pdf) 

**Diagnostic Assessment:**

1. **Residuals vs Fitted:** Should show random scatter around zero (no pattern)
2. **Q-Q Plot:** Points should follow the diagonal line (normality of residuals)
3. **Scale-Location:** Should show random scatter (homoscedasticity)
4. **Residuals vs Leverage:** Identifies influential observations

The diagnostics suggest:
- Residuals are approximately normally distributed
- Variance appears relatively constant (though some heteroscedasticity may be present)
- A few high-leverage points but none appear overly influential
- The log transformation has improved model assumptions

\newpage

# Summary and Conclusions

## Problem 1: Blood Sugar Analysis

Both the sign test (p = 0.8463) and Wilcoxon signed-rank test (p = 0.1447) failed to provide evidence that the median blood sugar is less than 120 mg/dL. The sample median of 118 mg/dL is not significantly different from 120.

## Problem 2: Brain Data Analysis  

While humans have a higher glia-neuron ratio (1.65) than predicted from the non-human primate relationship (1.459), this value falls within the 95% prediction interval [1.0059, 1.9125]. However, this analysis requires caution because:

- Human brain mass exceeds the range of non-human data (extrapolation)
- Unique human evolutionary adaptations may not follow the same relationship
- Limited sample size reduces precision

Therefore, we cannot conclusively determine if humans have an "excessive" glia-neuron ratio.

## Problem 3: Heart Disease Cost Analysis

The full multiple linear regression model is strongly preferred over the simple model:

**Key Findings:**

1. **ER Visits:** Each additional ER visit increases costs by 19.2% (p < 0.001)

2. **Complications:** Having complications increases costs by 344.6% (p < 0.001)—the strongest predictor

3. **Duration:** Each additional treatment day increases costs by 0.56% (p < 0.001)

4. **Age:** Surprisingly, older age is associated with slightly lower costs (p = 0.010)

5. **Gender:** Not a significant predictor after controlling for other factors

The multiple regression model explains 26.8% of variance in log-transformed costs, compared to only 10.3% for the simple model with ER visits alone.

\newpage

# Appendix: R Code

All R code used in this analysis is embedded in the R Markdown document. The complete, commented code can also be found in the accompanying R script file (`HW4_solutions.R`).

## Session Information


``` r
sessionInfo()
```

```
## R version 4.5.1 (2025-06-13)
## Platform: aarch64-apple-darwin20
## Running under: macOS Sequoia 15.5
## 
## Matrix products: default
## BLAS:   /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRblas.0.dylib 
## LAPACK: /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## time zone: America/New_York
## tzcode source: internal
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] e1071_1.7-16    patchwork_1.3.2 broom_1.0.9     knitr_1.50     
##  [5] readxl_1.4.5    lubridate_1.9.4 forcats_1.0.0   stringr_1.5.1  
##  [9] dplyr_1.1.4     purrr_1.1.0     readr_2.1.5     tidyr_1.3.1    
## [13] tibble_3.3.0    ggplot2_3.5.2   tidyverse_2.0.0
## 
## loaded via a namespace (and not attached):
##  [1] generics_0.1.4     class_7.3-23       stringi_1.8.7      lattice_0.22-7    
##  [5] hms_1.1.3          digest_0.6.37      magrittr_2.0.3     evaluate_1.0.5    
##  [9] grid_4.5.1         timechange_0.3.0   RColorBrewer_1.1-3 fastmap_1.2.0     
## [13] cellranger_1.1.0   Matrix_1.7-3       backports_1.5.0    mgcv_1.9-3        
## [17] scales_1.4.0       cli_3.6.5          crayon_1.5.3       rlang_1.1.6       
## [21] bit64_4.6.0-1      splines_4.5.1      withr_3.0.2        yaml_2.3.10       
## [25] parallel_4.5.1     tools_4.5.1        tzdb_0.5.0         vctrs_0.6.5       
## [29] R6_2.6.1           proxy_0.4-27       lifecycle_1.0.4    bit_4.6.0         
## [33] vroom_1.6.5        pkgconfig_2.0.3    pillar_1.11.0      gtable_0.3.6      
## [37] glue_1.8.0         xfun_0.53          tidyselect_1.2.1   rstudioapi_0.17.1 
## [41] farver_2.1.2       htmltools_0.5.8.1  nlme_3.1-168       rmarkdown_2.30    
## [45] labeling_0.4.3     compiler_4.5.1
```

---

**End of Report**
