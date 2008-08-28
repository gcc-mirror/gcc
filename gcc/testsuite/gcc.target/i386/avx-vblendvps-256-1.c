/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

#ifndef MASK
#define MASK 214
#endif

#define mask_v(pos) (((MASK >> (pos)) & 1U) << 31)

void static
avx_test (void)
{
  int i;
  union256 u, mask, s1, s2;
  int m[8]={mask_v(0), mask_v(1), mask_v(2), mask_v(3), 
            mask_v(4), mask_v(5), mask_v(6), mask_v(7)};
  float e [8];

  s1.x = _mm256_set_ps (34545, 95567, 23443, 5675, 2323, 67, 2345, 45667);
  s2.x = _mm256_set_ps (674, 57897, 93459, 45624, 54674, 1237, 67436, 79608);
  mask.x = _mm256_loadu_ps ((float *)m);

  u.x = _mm256_blendv_ps (s1.x, s2.x, mask.x);

  for (i = 0; i < 8; i++)
    e[i] = (m[i] & (1ULL << 31)) ? s2.a[i] : s1.a[i];

  if (check_union256 (u, e))
    abort ();
}
