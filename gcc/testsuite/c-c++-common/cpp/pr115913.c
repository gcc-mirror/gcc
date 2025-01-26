/* { dg-do preprocess } */
/* PR middle-end/115913 */
#pragma GCC push_options
#pragma GCC diagnostic warning "-Wundef"
/* The call to cl_optimization_compare performed by pop_options should not
   lead to a checking failure.  */
#pragma GCC pop_options
