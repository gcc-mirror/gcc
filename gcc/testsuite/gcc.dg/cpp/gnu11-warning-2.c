/* Test #warning not in C11.  */
/* { dg-do preprocess } */
/* { dg-options "-std=gnu11 -pedantic" } */

#warning example text /* { dg-warning "example text" } */
/* { dg-warning "#warning before C23 is a GCC extension" "pedantic" { target *-*-* } .-1 } */
