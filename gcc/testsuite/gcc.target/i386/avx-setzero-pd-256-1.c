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

  u.x = _mm256_setzero_pd ();

  for (i = 0; i < 4; i++)
    e[i] = 0.0;

  if (check_union256d (u, e))
    abort ();
}
