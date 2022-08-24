/* Test #warning in C2x.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c2x -pedantic-errors" } */

#warning example text /* { dg-warning "example text" } */
