/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  union {
    float f[8];
    int   i[8];
  }source1, source2, e;
  
  int i;
  union256 u, s1, s2;

  s1.x = _mm256_set_ps (34545, 95567, 23443, 5675, 2323, 67, 2345, 45667);
  s2.x = _mm256_set_ps (674, 57897, 93459, 45624, 54674, 1237, 67436, 79608);
  
  _mm256_storeu_ps (source1.f, s1.x);
  _mm256_storeu_ps (source2.f, s2.x);

  u.x = _mm256_xor_ps (s1.x, s2.x);

  for (i = 0; i < 8; i++)
    e.i[i] = source1.i[i] ^ source2.i[i];

  if (check_union256 (u, e.f))
    abort ();
}
