/* Test #warning not in C11.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c11 -Wc11-c23-compat" } */

#warning example text /* { dg-warning "example text" } */
/* { dg-warning "#warning before C23 is a GCC extension" "compat" { target *-*-* } .-1 } */
