/* { dg-do run { target *-*-linux* } } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */

#include "sse2-check.h"

extern void abort (void);

typedef _Complex float __attribute__((mode(TC))) _Complex128;

_Complex128 __attribute__ ((noinline))
foo (_Complex128 x, _Complex128 y)
{
  return x * y;
}

static void
sse2_test (void)
{
  _Complex128 a = 1.3q + 3.4qi, b = 5.6q + 7.8qi, c;

  c = foo (a, b);
  if (__real__(c) == 0.0q || __imag__ (c) == 0.0q)
    abort ();
}
