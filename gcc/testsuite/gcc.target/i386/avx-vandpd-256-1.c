/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  int i;
  union256d u, s1, s2;

  union
  {
     double d[4];
     long long ll[4];
  }source1, source2, e;

  s1.x = _mm256_set_pd (345.45, 95567, 2344.3, 567.5);
  s2.x = _mm256_set_pd (674, 57.897, 934.59, 4562.4);

  _mm256_storeu_pd (source1.d, s1.x);
  _mm256_storeu_pd (source2.d, s2.x);

  u.x = _mm256_and_pd (s1.x, s2.x);

  for (i = 0; i < 4; i++)
    e.ll[i] = source1.ll[i] & source2.ll[i];

  if (check_union256d (u, e.d))
    abort ();
}
