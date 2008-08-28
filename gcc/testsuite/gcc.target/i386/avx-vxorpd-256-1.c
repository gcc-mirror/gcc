/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  union
  {
    double d[4];
    long long l[4];
  }source1, source2, e;

  int i;
  union256d u, s1, s2;

  s1.x = _mm256_set_pd (34545.123, 95567.456, 23443.09876, 5675.543);
  s2.x = _mm256_set_pd (674, 57897.332187, 93459, 45624.112);
  _mm256_storeu_pd (source1.d, s1.x);
  _mm256_storeu_pd (source2.d, s2.x);

  u.x = _mm256_xor_pd (s1.x, s2.x);

  for (i = 0; i < 4; i++)
    e.l[i] = source1.l[i] ^ source2.l[i];

  if (check_union256d (u, e.d))
    abort ();
}
