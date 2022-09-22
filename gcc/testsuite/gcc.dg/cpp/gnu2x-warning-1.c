/* Test #warning in C2x.  */
/* { dg-do preprocess } */
/* { dg-options "-std=gnu2x -pedantic-errors" } */

#warning example text /* { dg-warning "example text" } */
