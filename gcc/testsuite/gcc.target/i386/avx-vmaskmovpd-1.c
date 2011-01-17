/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

#ifndef MASK
#define MASK 7
#endif

#define mask_v(pos) (((MASK & (0x1ULL << (pos))) >> (pos)) << 63)

void static
avx_test (void)
{
  int i;
  long long m[2] = {mask_v(0), mask_v(1)};
  double s[2] = {1.1, 2.2};
  union128d u;
  union128i_q mask;
  double e[2] = {0.0};

  mask.x = _mm_loadu_si128 ((__m128i *)m);
  u.x = _mm_maskload_pd (s, mask.x);

  for (i = 0 ; i < 2; i++) 
    e[i] = m[i] ? s[i] : 0;
   
  if (check_union128d (u, e))
    abort ();
}
