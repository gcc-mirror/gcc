/* { dg-do run } */
/* { dg-options "-O0 -mlong-double-64 -mfpmath=sse -msse2" } */
/* { dg-require-effective-target sse2 } */

#include "sse2-check.h"

static void
sse2_test (void)
{
  __float128 a = -0.23456789;
  if ((double) a >= 0)
    __builtin_abort ();
}
