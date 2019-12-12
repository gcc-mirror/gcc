/* Test that _Decimal* keywords are accepted in C2X mode: compat warnings.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -Wc11-c2x-compat" } */

_Decimal32 d32; /* { dg-warning "ISO C does not support" } */
_Decimal64 d64; /* { dg-warning "ISO C does not support" } */
_Decimal128 d128; /* { dg-warning "ISO C does not support" } */
