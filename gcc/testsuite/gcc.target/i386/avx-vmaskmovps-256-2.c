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
  int m[8] = {mask_v(0), mask_v(1), mask_v(2), mask_v(3), mask_v(4), mask_v(5), mask_v(6), mask_v(7)};
  float s[8] = {1,2,3,4,5,6,7,8};
  union256 src;
  union256i_d mask;
  float e [8] = {0.0};
  float d [8] = {0.0};

  src.x = _mm256_loadu_ps (s);
  mask.x = _mm256_loadu_si256 ((__m256i *)m);
  _mm256_maskstore_ps (d, mask.x, src.x);

  for (i = 0 ; i < 8; i++) 
    e[i] = m[i] ? s[i] : 0;
   
  if (checkVf (d, e, 8))
    abort ();
}
