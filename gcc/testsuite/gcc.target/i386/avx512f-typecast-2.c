/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

extern int memcmp (const void *, const void *, __SIZE_TYPE__);

void
avx512f_test (void)
{
  union512i_d ad, zero;
  union512 bd;
  union512d cd;
  union256i_d d;
  union256 e;
  union256d f;
  union128i_d g;
  union128 h;
  union128d k;
  int i;

  for (i = 0; i < 16; i++)
    zero.a[i] = 0;

  for (i = 0; i < 8; i++)
    {
      d.a[i] = 109534 + i;
      e.a[i] = 85034.095f + i;
    }

  for (i = 0; i < 4; i++)
    {
      f.a[i] = 41234512451345.0905 + i;
      g.a[i] = 71469086341 + i;
      h.a[i] = 45.1264f + i;
    }

  for (i = 0; i < 2; i++)
    k.a[i] = 7146908634.576 + i;

  cd.x = _mm512_zextpd128_pd512 (k.x);
  if (memcmp (cd.a, k.a, 16)
      || memcmp (&cd.a[2], &zero.a, 48))
    abort ();

  bd.x = _mm512_zextps128_ps512 (h.x);
  if (memcmp (bd.a, h.a, 16)
      || memcmp (&bd.a[4], &zero.a, 48))
    abort ();

  ad.x = _mm512_zextsi128_si512 (g.x);
  if (memcmp (ad.a, g.a, 16)
      || memcmp (&ad.a[4], &zero.a, 48))
    abort ();

  cd.x = _mm512_zextpd256_pd512 (f.x);
  if (memcmp (cd.a, f.a, 32)
      || memcmp (&cd.a[4], &zero.a, 32))
    abort ();

  bd.x = _mm512_zextps256_ps512 (e.x);
  if (memcmp (bd.a, e.a, 32)
      || memcmp (&bd.a[8], &zero.a, 32))
    abort ();

  ad.x = _mm512_zextsi256_si512 (d.x);
  if (memcmp (ad.a, d.a, 32)
      || memcmp (&ad.a[8], &zero.a, 32))
    abort ();
}
