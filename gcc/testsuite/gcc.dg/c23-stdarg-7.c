/* Test C23 variadic functions with no named parameters, or last named
   parameter with a declaration not allowed in C17.  Execution tests.  */
/* { dg-do run } */
/* { dg-options "-O2 -std=c23 -pedantic-errors" } */

#include "c23-stdarg-4.c"

/* { dg-warning "'va_start' macro used with additional arguments other than identifier of the last named argument" "" { target *-*-* } 0 } */
