/* { dg-do run } */
/* { dg-options "-O2 -mavx" } */
/* { dg-require-effective-target avx } */

#include "avx-check.h"

extern int memcmp (const void *, const void *, __SIZE_TYPE__);

void
avx_test (void)
{
  union256i_d ad, zero;
  union256 bd;
  union256d cd;
  union128i_d d;
  union128 e;
  union128d f;
  int i;

  for (i = 0; i < 8; i++)
    zero.a[i] = 0;

  for (i = 0; i < 4; i++)
    {
      d.a[i] = 109534 + i;
      e.a[i] = 85034.095f + i;
    }

  for (i = 0; i < 2; i++)
    f.a[i] = 41234512451345.0905 + i;

  cd.x = _mm256_zextpd128_pd256 (f.x);
  if (memcmp (cd.a, f.a, 16)
      || memcmp (&cd.a[2], &zero.a, 16))
    abort ();

  bd.x = _mm256_zextps128_ps256 (e.x);
  if (memcmp (bd.a, e.a, 16)
      || memcmp (&bd.a[4], &zero.a, 16))
    abort ();

  ad.x = _mm256_zextsi128_si256 (d.x);
  if (memcmp (ad.a, d.a, 16)
      || memcmp (&ad.a[4], &zero.a, 16))
    abort ();
}
