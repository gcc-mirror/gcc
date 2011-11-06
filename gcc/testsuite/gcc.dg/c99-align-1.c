/* Test _Alignof and _Alignas not in C99.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

int a = _Alignof (int); /* { dg-error "ISO C99 does not support '_Alignof'" } */
_Alignas (int) int b; /* { dg-error "ISO C99 does not support '_Alignas'" } */
