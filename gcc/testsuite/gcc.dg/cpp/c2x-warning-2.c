/* Test #warning in C2x: -Wc11-c2x-comapt.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c2x -pedantic-errors -Wc11-c2x-compat" } */

#warning example text /* { dg-warning "example text" } */
/* { dg-warning "#warning before C2X is a GCC extension" "compat" { target *-*-* } .-1 } */
