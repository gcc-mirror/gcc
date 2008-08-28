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
  long long m[4] = {mask_v(0), mask_v(1), mask_v(2), mask_v(3)};
  double s[4] = {1.1, 2.2, 3.3, 4.4};
  double e [4] = {0.0};
  double d [4] = {0.0};
  union256d src, mask;
  
  src.x = _mm256_loadu_pd (s);
  mask.x = _mm256_loadu_pd ((double*)m);
  _mm256_maskstore_pd (d, mask.x, src.x);

  for (i = 0 ; i < 4; i++) 
    e[i] = m[i] ? s[i] : 0;
   
  if (checkVd (d, e, 4))
    abort ();
}
