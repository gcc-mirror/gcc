/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

#ifndef MASK
#define MASK 6
#endif

#define mask_v(pos) (((MASK & (0x1ULL << (pos))) >> (pos)) << 63)

void static
avx_test (void)
{
  int i;
  long long m[2] = {mask_v(0), mask_v(1)};
  double s[2] = {1.1, 2.2};
  double e[2] = {0.0};
  double d[2] = {0.0};
  union128d src;
  union128i_q mask;
  
  src.x = _mm_loadu_pd (s);
  mask.x = _mm_loadu_si128 ((__m128i *)m);
  _mm_maskstore_pd (d, mask.x, src.x);

  for (i = 0 ; i < 2; i++) 
    e[i] = m[i] ? s[i] : 0;
   
  if (checkVd (d, e, 2))
    abort ();
}
