/* Test #warning not in C11.  */
/* { dg-do preprocess } */
/* { dg-options "-std=gnu11 -Wc11-c2x-compat" } */

#warning example text /* { dg-warning "example text" } */
/* { dg-warning "#warning before C2X is a GCC extension" "compat" { target *-*-* } .-1 } */
