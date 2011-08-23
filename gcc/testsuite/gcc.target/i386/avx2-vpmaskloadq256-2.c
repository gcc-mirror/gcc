/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include "avx2-check.h"

#ifndef MASK
#define MASK 7
#endif

#define mask_v(pos) (((MASK & (0x1ULL << (pos))) >> (pos)) << 63)

void static
avx2_test (void)
{
  int i;
  long long m[4] = { mask_v (0), mask_v (1), mask_v (2), mask_v (3) };
  long long s[4] = { 1, 2, 3, 4 };
  union256i_q u, mask;
  long long e[4] = { 0 };

  mask.x = _mm256_loadu_si256 ((__m256i *) m);
  u.x = _mm256_maskload_epi64 (s, mask.x);

  for (i = 0; i < 4; i++)
    e[i] = m[i] ? s[i] : 0;

  if (check_union256i_q (u, e))
    abort ();
}
