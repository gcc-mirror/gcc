/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  int i;
  union256 u, s1, s2;
  int source1[8]={34545, 95567, 23443, 5675, 2323, 67, 2345, 45667};
  int source2[8]={674, 57897, 93459, 45624, 54674, 1237, 67436, 79608};
  int d[8];
  int e[8];

  s1.x = _mm256_loadu_ps ((float *)source1);
  s2.x = _mm256_loadu_ps ((float *)source2);
  u.x = _mm256_andnot_ps (s1.x, s2.x);

  _mm256_storeu_ps ((float *)d, u.x);

  for (i = 0; i < 8; i++)
    e[i] = (~source1[i]) & source2[i];

  if (checkVi (d, e, 8))
    abort ();
}
