/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include "avx2-check.h"

#ifndef MASK
#define MASK 6
#endif

#define mask_v(pos) (((MASK & (0x1ULL << (pos))) >> (pos)) << 63)

void static
avx2_test (void)
{
  int i;
  long long m[4] = { mask_v (0), mask_v (1), mask_v (2), mask_v (3) };
  long long s[4] = { 1, 2, 3, 4 };
  long long e[4] = { 0 };
  long long d[4] = { 0 };
  union256i_q src, mask;

  src.x = _mm256_loadu_si256 ((__m256i *) s);
  mask.x = _mm256_loadu_si256 ((__m256i *) m);
  _mm256_maskstore_epi64 (d, mask.x, src.x);

  for (i = 0; i < 4; i++)
    e[i] = m[i] ? s[i] : 0;

  if (checkVl (d, e, 4))
    abort ();
}
