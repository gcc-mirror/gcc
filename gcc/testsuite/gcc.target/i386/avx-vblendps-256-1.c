/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

#ifndef MASK
#define MASK 114
#endif

void static
avx_test (void)
{
  int i;
  union256 u, s1, s2;
  float e [8];

  s1.x = _mm256_set_ps (34545, 95567, 23443, 5675, 2323, 67, 2345, 45667);
  s2.x = _mm256_set_ps (674, 57897, 93459, 45624, 54674, 1237, 67436, 79608);
  u.x = _mm256_blend_ps (s1.x, s2.x, MASK);

  for (i = 0; i < 8; i++)
    e[i] = (MASK & (0x01 << i)) ? s2.a[i] : s1.a[i];

  if (check_union256 (u, e))
    abort ();
}
