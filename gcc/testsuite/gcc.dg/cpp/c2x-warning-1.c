/* Test #warning in C23.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#warning example text /* { dg-warning "example text" } */
