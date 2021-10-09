/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */

#include "sse4_1-check.h"

#include "pr102483.c"

static void
sse4_1_test ()
{
  char p[4] = { -103, 23, 41, -56 };
  unsigned char up[4] = { 100, 30, 255, 9 };

  char res = reduce_add (p);
  if (res != -95)
    abort ();
  if (reduce_smin (p) != -103)
    abort ();
  if (reduce_smax (p) != 41)
    abort ();
  if (reduce_umin (up) != 9)
    abort ();
  if (reduce_umax (up) != 255)
    abort();
}
