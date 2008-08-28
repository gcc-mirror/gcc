/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

#define CONTROL 5

void static
avx_test (void)
{
  union256d u, s1;
  double e [4];

  s1.x = _mm256_set_pd (2134.3343,1234.635654,453.345635,54646.464356);
  u.x = _mm256_permute_pd (s1.x, CONTROL);

  e[0] = (CONTROL&0x01) ? s1.a[1] : s1.a[0];
  e[1] = (CONTROL&0x02) ? s1.a[1] : s1.a[0];
  e[2] = (CONTROL&0x04) ? s1.a[3] : s1.a[2];
  e[3] = (CONTROL&0x08) ? s1.a[3] : s1.a[2];

  if (check_union256d (u, e))
    abort ();
}
