/* PR tree-optimization/80631 */
/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -mavx2 -fno-vect-cost-model" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

#define N 8

static void
avx2_test (void)
{
  int v[N], k;
  for(k = 0; k < N; k++)
    v[k] = k;
  v[0] = 77;
  int found_index = -1;
  for (k = 0; k < N; k++)
    if (v[k] == 77)
      found_index = k;
  if (found_index != 0)
    abort ();
}
