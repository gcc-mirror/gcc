/* Test #warning in C23: -Wc11-c2x-comapt.  */
/* { dg-do preprocess } */
/* { dg-options "-std=gnu2x -pedantic-errors -Wc11-c2x-compat" } */

#warning example text /* { dg-warning "example text" } */
/* { dg-warning "#warning before C23 is a GCC extension" "compat" { target *-*-* } .-1 } */
