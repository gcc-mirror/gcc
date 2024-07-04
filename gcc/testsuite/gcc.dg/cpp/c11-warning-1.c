/* Test #warning not in C11.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#warning example text /* { dg-warning "example text" } */
/* { dg-error "#warning before C23 is a GCC extension" "pedantic" { target *-*-* } .-1 } */
