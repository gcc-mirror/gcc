/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  int i;
  union256d u;
  double e [4];
  double source[1] = {26156.643};

  u.x = _mm256_set1_pd (source[0]);

  for (i = 0; i < 4; i++)
    e[i] = source[0];

  if (check_union256d (u, e))
    abort ();
}
