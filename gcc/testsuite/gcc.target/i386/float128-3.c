/* { dg-do run } */
/* { dg-options "-O2 -msse4.1" } */
/* { dg-require-effective-target sse4 } */

#include "sse4_1-check.h"

int signbit (__float128);

extern void abort (void);

static void
sse4_1_test (void)
{
  static volatile __float128 a;

  a = -1.2q;
  if (!signbit (a))
    abort ();

  a = 1.2q;
  if (signbit (a))
    abort ();
}
