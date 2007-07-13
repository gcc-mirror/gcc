/* Test for <tgmath.h> in C99. */
/* Origin: Matt Austern <austern@apple.com>
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999" } */

/* Test that invoking type-generic exp on a complex invokes cexp. */
#include <tgmath.h>

complex double foo(complex double x)
{
  return exp(x);
}

/* {dg-final {scan-assembler "cexp" } } */
