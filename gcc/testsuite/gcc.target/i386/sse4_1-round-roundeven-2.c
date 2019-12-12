/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1 -mfpmath=sse" } */

#include "sse4_1-check.h"
#include "sse4_1-round-roundeven-1.c"

static void
sse4_1_test (void)
{
  if (f1 (0.5) != 0.0 || f1 (1.5) != 2.0 || f1 (-0.5) != 0.0 || f1 (-1.5) != -2.0)
    abort ();
  if (f2 (0.5f) != 0.0f || f2 (1.5f) != 2.0f || f2 (-0.5f) != 0.0f || f2 (-1.5f) != -2.0f)
    abort ();
}
