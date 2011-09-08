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
  long long m[2] = { mask_v (0), mask_v (1) };
  long long s[2] = { 1, 2 };
  long long e[2] = { 0 };
  long long d[2] = { 0 };
  union128i_q src, mask;

  src.x = _mm_loadu_si128 ((__m128i *) s);
  mask.x = _mm_loadu_si128 ((__m128i *) m);
  _mm_maskstore_epi64 (d, mask.x, src.x);

  for (i = 0; i < 2; i++)
    e[i] = m[i] ? s[i] : 0;

  if (checkVl (d, e, 2))
    abort ();
}
