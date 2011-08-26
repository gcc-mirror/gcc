/* Test _Noreturn not in C99.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

_Noreturn void f (void); /* { dg-error "ISO C99 does not support '_Noreturn'" } */
