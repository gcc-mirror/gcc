/* Test that _Decimal* keywords diagnosed in C11 mode: -pedantic.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic" } */

_Decimal32 d32; /* { dg-warning "ISO C does not support" } */
_Decimal64 d64; /* { dg-warning "ISO C does not support" } */
_Decimal128 d128; /* { dg-warning "ISO C does not support" } */
