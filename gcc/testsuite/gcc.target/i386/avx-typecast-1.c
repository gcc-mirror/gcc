/* { dg-do run } */
/* { dg-options "-O2 -mavx" } */
/* { dg-require-effective-target avx } */

#include "avx-check.h"

extern int memcmp (const void *, const void *, __SIZE_TYPE__);

void
avx_test (void)
{
  union256i_d  a, ad;
  union256  b, bd;
  union256d  c, cd;
  union128i_d  d, dd;
  union128  e, ed;
  union128d  f, fd;
  int i;

  for (i = 0; i < 8; i++)
    {
      a.a[i] = 7146908634 + i;
      b.a[i] = 45.12f + i;
    }

  for (i = 0; i < 4; i++)
    {
      c.a[i] = 41234512513451345.0905 + i;
      d.a[i] = 109534 + i;
      e.a[i] = 85034.095f + i;
    }

  for (i = 0; i < 2; i++)
    f.a[i] = 41234512451345.0905 + i;

  bd.x = _mm256_castpd_ps (c.x);
  if (memcmp (bd.a, c.a, 32))
    abort ();

  ad.x = _mm256_castpd_si256 (c.x);
  if (memcmp (ad.a, c.a, 32))
    abort ();

  cd.x = _mm256_castps_pd (b.x);
  if (memcmp (cd.a, b.a, 32))
    abort ();

  ad.x = _mm256_castps_si256 (b.x);
  if (memcmp (ad.a, b.a, 32))
    abort ();

  bd.x = _mm256_castsi256_ps (a.x);
  if (memcmp (bd.a, a.a, 32))
    abort ();

  cd.x = _mm256_castsi256_pd (a.x);
  if (memcmp (cd.a, a.a, 32))
    abort ();

  fd.x = _mm256_castpd256_pd128 (c.x);
  if (memcmp (fd.a, c.a, 16))
    abort ();

  ed.x = _mm256_castps256_ps128 (b.x);
  if (memcmp (ed.a, b.a, 16))
    abort ();

  dd.x = _mm256_castsi256_si128 (a.x);
  if (memcmp (dd.a, a.a, 16))
    abort ();

  cd.x = _mm256_castpd128_pd256 (f.x);
  if (memcmp (cd.a, f.a, 16))
    abort ();

  bd.x = _mm256_castps128_ps256 (e.x);
  if (memcmp (bd.a, e.a, 16))
    abort ();

  ad.x = _mm256_castsi128_si256 (d.x);
  if (memcmp (ad.a, d.a, 16))
    abort ();
}
