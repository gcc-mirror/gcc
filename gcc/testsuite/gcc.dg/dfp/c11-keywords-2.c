/* Test that _Decimal* keywords diagnosed in C11 mode: -pedantic-errors.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

_Decimal32 d32; /* { dg-error "ISO C does not support" } */
_Decimal64 d64; /* { dg-error "ISO C does not support" } */
_Decimal128 d128; /* { dg-error "ISO C does not support" } */
