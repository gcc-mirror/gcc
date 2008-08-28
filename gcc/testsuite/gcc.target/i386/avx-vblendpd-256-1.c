/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

#ifndef MASK
#define MASK 12
#endif

void static
avx_test (void)
{
  int i;
  union256d u, s1, s2;
  double e [4];

  s1.x = _mm256_set_pd (34545, 95567, 23443, 5675);
  s2.x = _mm256_set_pd (674, 57897, 93459, 45624);
  u.x = _mm256_blend_pd (s1.x, s2.x, MASK);

  for (i = 0; i < 4; i++)
    e[i] = (MASK & (0x01 << i)) ? s2.a[i] : s1.a[i];

  if (check_union256d (u, e))
    abort ();
}
