/* Test that DFP constants and _Decimal64x keyword are diagnosed in C11 mode: -pedantic.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic" } */

int a = (int) 1.1D64x; /* { dg-warning "C23 feature" } */
int b = (int) 2.d64x; /* { dg-warning "C23 feature" } */
_Decimal64x c = 1; /* { dg-warning "ISO C does not support decimal floating-point before C23" } */
