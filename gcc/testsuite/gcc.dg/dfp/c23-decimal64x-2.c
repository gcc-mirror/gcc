/* Test that DFP constants and _Decimal64x keyword are accepted in C23 mode: compat warnings.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -Wc11-c23-compat" } */

int a = (int) 1.1D64x; /* { dg-warning "C23 feature" } */
int b = (int) 2.d64x; /* { dg-warning "C23 feature" } */
_Decimal64x c = 1; /* { dg-warning "ISO C does not support decimal floating-point before C23" } */
