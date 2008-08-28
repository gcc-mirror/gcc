/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

#ifndef MASK
#define MASK 134
#endif

#define mask_v(pos) (((MASK & (0x1 << (pos))) >> (pos)) << 31)

void static
avx_test (void)
{
  int i;
  int m[8] = {mask_v(0), mask_v(1), mask_v(2), mask_v(3), mask_v(4), mask_v(5), mask_v(6), mask_v(7)};
  float s[8] = {1,2,3,4,5,6,7,8};
  union256 u, mask;
  float e [8] = {0.0};

  mask.x = _mm256_loadu_ps ((float*)m);
  u.x = _mm256_maskload_ps (s, mask.x);

  for (i = 0 ; i < 8; i++) 
    e[i] = m[i] ? s[i] : 0;
   
  if (check_union256 (u, e))
    abort ();
}
