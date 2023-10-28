Function of Summarizing Groups
================
Ivy Liu
2023-10-28

## Exercise 1: Make a Function (25 points)

## Exercise 2: Document your Function (20 points)

In this exercise, we will make a function that take a data frame or a
tibble, and output the summary of data after grouping.

``` r
#' @title Summary of Groups
#' @details This function output a summary table after grouping of your data.
#' @param data the dataset you want to do analysis on
#' @param group_cols a vector of column names where the grouping occurs
#' @param summary_cols a vector of variable names to perform statistical summary upon
#' @param summary_functions a list summarizing functions
#' @return returns a summary table with the output of the summarizing functions after grouping

group_summary <- function(data, group_cols, summary_cols, summary_functions) {
  for(i in summary_cols){
    if(any(is.na(data[which(names(data) == i)]))){
      stop("Sorry, this function only works for data with no NA entry.")
    }
  }
  data %>%
    group_by(across({{group_cols}})) %>%
    summarize(across(all_of({{summary_cols}}),
                     .fns = summary_functions,
                     .names = "{.col}_{.fn}"),
              .groups = "keep")
}
```

## Exercise 3: Include examples (15 points)

We begin with a simple example where the function only compute the mean
after grouping.

``` r
mydata1 = tibble(group = c("A", "A", "B", "B"),
                number = c(1,2,3,4))
group_summary(mydata1, group, summary_cols = "number", list(Mean = mean))
```

    ## # A tibble: 2 × 2
    ## # Groups:   group [2]
    ##   group number_Mean
    ##   <chr>       <dbl>
    ## 1 A             1.5
    ## 2 B             3.5

The output is what we expected because it correctly computes the mean of
number in group A and group B.

Now we move on to a slightly more complicated example, where we perform
grouping by two categorical variables, and perform two summarizing
functions on two numerical variables.

``` r
mydata2 = tibble(group1 = c("A", "A", "A", "A", "B", "B", "B", "B"),
                 group2 = c("C", "C", "D", "D", "C", "C", "D", "D"),
                 number1= c(5,5,6,6,7,7,8,8),
                 number2 = c(1, 1, 2, 2, 3, 3, 4, 4))
group_summary(data = mydata2, group_col = c(group1, group2), 
              summary_cols = c("number1", "number2"), 
              list(Mean = mean, Median = median))
```

    ## # A tibble: 4 × 6
    ## # Groups:   group1, group2 [4]
    ##   group1 group2 number1_Mean number1_Median number2_Mean number2_Median
    ##   <chr>  <chr>         <dbl>          <dbl>        <dbl>          <dbl>
    ## 1 A      C                 5              5            1              1
    ## 2 A      D                 6              6            2              2
    ## 3 B      C                 7              7            3              3
    ## 4 B      D                 8              8            4              4

The result is also expected. The function first splits the data into
four groups (AC, AD, BC, and BD), and then compute the mean and median
of both number1 and number2 in each group.

There is one drawback of the customized function: this function cannot
perform the count function “n()” as a summary because it defaultly
passes the colnames in “summary_cols” into the summary function,
i.e. “n(number1)”, which is an invalid syntax.

``` r
group_summary(data = mydata2, group_col = c(group1), 
              summary_cols = c("number1"), 
              list(Count = n))
```

    ## Error in `summarize()`:
    ## ! Problem while computing `..1 = across(all_of("number1"), .fns =
    ##   summary_functions, .names = "{.col}_{.fn}")`.
    ## ℹ The error occurred in group 1: group1 = "A".
    ## Caused by error in `across()`:
    ## ! Problem while computing column `number1_Count`.
    ## Caused by error:
    ## ! 参数没有用(number1)

## Exercise 4: Test the Function (25 points)

``` r
# test of a normal dataset
test_data = tibble(group1 = rep(c(1,2), each = 4),
                   value = rep(c(1,2), each = 4))
test_that("Testing the numeric output", {
  expect_equal(group_summary(data = test_data, group_col = c(group1), 
              summary_cols = c("value"), 
              list(Mean = mean))$value_Mean, c(1,2) )
  expect_equal(group_summary(data = test_data, group_col = c(group1), 
              summary_cols = c("value"), 
              list(Mean = mean))$group1, c(1,2))
})
```

    ## Test passed 😀

``` r
# test of a dataset with NA's
test_data_na = tibble(group1 = rep(c(1,2), each = 4),
                   value = rep(c(1, NA, 2, NA), each = 2))
test_that("Testing the numeric output", {
  expect_error(group_summary(data = test_data_na, group_col = c(group1), 
              summary_cols = c("value"), list(Mean = mean)), 
              "Sorry, this function only works for data with no NA entry.")
})
```

    ## Test passed 🎊

``` r
# test of a multiple grouping on a data frame type of input
test_data_2_grouping = data.frame(group1 = rep(c(1,2), each = 4),
                                  group2 = rep(c(3,4), times = 4),
                                  value = c(1,2,3,4,5,6,7,8))
test_that("Testing multiple grouping", {
  expect_equal(colnames(group_summary(data = test_data_2_grouping, 
                             group_col = c(group1,group2), 
                             summary_cols = c("value"), 
                             list(Mean = mean, Median = median))), 
               c("group1", "group2", "value_Mean", "value_Median"))
  expect_equal(group_summary(data = test_data_2_grouping, 
                             group_col = c(group1,group2), 
                             summary_cols = c("value"), 
                             list(Mean = mean, Median = median))$value_Mean, 
               c(2, 3, 6, 7))
  expect_equal(group_summary(data = test_data_2_grouping, 
                             group_col = c(group1,group2), 
                             summary_cols = c("value"), 
                             list(Mean = mean, Median = median))$value_Median, 
               c(2, 3, 6, 7))
})
```

    ## Test passed 🎊
