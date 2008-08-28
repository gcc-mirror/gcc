/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

#ifndef MASK
#define MASK 6
#endif

#define mask_v(pos) (((MASK >> (pos)) & 1ULL) << 63)

void static
avx_test (void)
{
  int i;
  union256d u, mask, s1, s2;
  long long m[4]={mask_v(0), mask_v(1), mask_v(2), mask_v(3)};
  double e [4];

  s1.x = _mm256_set_pd (34545, 95567, 23443, 5675);
  s2.x = _mm256_set_pd (674, 57897, 93459, 45624);
  mask.x = _mm256_set_pd (m[0], m[1], m[2], m[3]);

  u.x = _mm256_blendv_pd (s1.x, s2.x, mask.x);

  for (i = 0; i < 4; i++)
    e[i] = (m[i] & (1ULL << 63)) ? s2.a[i] : s1.a[i];

  if (check_union256d (u, e))
    abort ();
}
