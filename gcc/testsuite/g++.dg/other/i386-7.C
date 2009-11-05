/* Test that x86intrin.h is usable with -O -pedantic-errors.  */
/* We were using SSE4.2 builtins without the extension available.  */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O -pedantic-errors" } */

#include <x86intrin.h>

int dummy;
