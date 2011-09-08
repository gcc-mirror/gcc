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
  long long m[2] = { mask_v (0), mask_v (1) };
  long long s[2] = { 1, 2 };
  union128i_q u, mask;
  long long e[2] = { 0 };

  mask.x = _mm_loadu_si128 ((__m128i *) m);
  u.x = _mm_maskload_epi64 (s, mask.x);

  for (i = 0; i < 2; i++)
    e[i] = m[i] ? s[i] : 0;

  if (check_union128i_q (u, e))
    abort ();
}
