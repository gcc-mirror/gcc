/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include "avx2-check.h"

#ifndef MASK
#define MASK 214
#endif

#define mask_v(pos) (((MASK & (0x1 << (pos))) >> (pos)) << 31)

void static
avx2_test (void)
{
  int i;
  int m[8] =
    { mask_v (0), mask_v (1), mask_v (2), mask_v (3), mask_v (4), mask_v (5),
mask_v (6), mask_v (7) };
  int s[8] = { 1, 2, 3, 4, 5, 6, 7, 8 };
  union256i_d src, mask;
  int e[8] = { 0 };
  int d[8] = { 0 };

  src.x = _mm256_loadu_si256 ((__m256i *) s);
  mask.x = _mm256_loadu_si256 ((__m256i *) m);
  _mm256_maskstore_epi32 (d, mask.x, src.x);

  for (i = 0; i < 8; i++)
    e[i] = m[i] ? s[i] : 0;

  if (checkVi (d, e, 8))
    abort ();
}
