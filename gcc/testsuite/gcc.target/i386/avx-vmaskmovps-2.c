/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

#ifndef MASK
#define MASK 214
#endif

#define mask_v(pos) (((MASK & (0x1 << (pos))) >> (pos)) << 31)

void static
avx_test (void)
{
  int i;
  int m[4] = {mask_v(0), mask_v(1), mask_v(2), mask_v(3)};
  float s[4] = {1,2,3,4};
  union128 src;
  union128i_d mask;
  float e[4] = {0.0};
  float d[4] = {0.0};

  src.x = _mm_loadu_ps (s);
  mask.x = _mm_loadu_si128 ((__m128i *)m);
  _mm_maskstore_ps (d, mask.x, src.x);

  for (i = 0 ; i < 4; i++) 
    e[i] = m[i] ? s[i] : 0;
   
  if (checkVf (d, e, 4))
    abort ();
}
