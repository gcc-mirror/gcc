/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -msse2" } */
/* { dg-require-effective-target sse2 } */

#include "sse2-check.h"

#define N 8

unsigned int u4[N] = { 4000000000u, 4000000000u, 4000000000u, 4000000000u, 4000000000u, 4000000000u, 4000000000u, 4000000000u };
float f4[N];

static void
sse2_test (void)
{
  int j;

  for (j = 0; j < N; j++)
    f4[j] = u4[j];

  /* check results:  */
  for (j = 0; j < N; j++)
    if (f4[j] != 4000000000.0)
      abort ();
}
