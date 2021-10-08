/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */

#include "sse4_1-check.h"

#include "mmx-reduce-op-1.c"

static void
sse4_1_test ()
{
  short p[4] = { -103, 23, 41, 200 };
  unsigned short up[4] = { 100, 30, 299, 1000 };

  if (reduce_add (p) != 161)
    abort ();
  if (reduce_smin (p) != -103)
    abort ();
  if (reduce_smax (p) != 200)
    abort ();
  if (reduce_umin (up) != 30)
    abort ();
  if (reduce_umax (up) != 1000)
    abort();
}
