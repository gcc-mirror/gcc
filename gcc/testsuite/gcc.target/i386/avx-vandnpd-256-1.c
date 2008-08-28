/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  int i;
  union256d u, s1, s2;
  long long source1[4]={34545, 95567, 23443, 5675};
  long long source2[4]={674, 57897, 93459, 45624};
  long long d[4];
  long long e[4];

  s1.x = _mm256_loadu_pd ((double *)source1);
  s2.x = _mm256_loadu_pd ((double *)source2);
  u.x = _mm256_andnot_pd (s1.x, s2.x);

  _mm256_storeu_pd ((double *)d, u.x);

  for (i = 0; i < 4; i++)
    e[i] = (~source1[i]) & source2[i];

  if (checkVl (d, e, 4))
    abort ();
}
