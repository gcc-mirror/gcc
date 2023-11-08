/* Test #warning in C23: -Wc11-c23-compat.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c23 -pedantic-errors -Wc11-c23-compat" } */

#warning example text /* { dg-warning "example text" } */
/* { dg-warning "#warning before C23 is a GCC extension" "compat" { target *-*-* } .-1 } */
