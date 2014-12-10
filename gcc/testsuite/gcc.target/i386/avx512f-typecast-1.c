/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

extern int memcmp (const void *, const void *, __SIZE_TYPE__);

void
avx512f_test (void)
{
  union512i_d  a, ad;
  union512  b, bd;
  union512d  c, cd;
  union256i_d  d, dd;
  union256  e, ed;
  union256d  f, fd;
  union128i_d  g, gd;
  union128  h, hd;
  union128d  k, kd;
  int i;

  for (i = 0; i < 16; i++)
    {
      a.a[i] = 7146908634 + i;
      b.a[i] = 45.12f + i;
    }

  for (i = 0; i < 8; i++)
    {
      c.a[i] = 41234512513451345.0905 + i;
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
    {
      k.a[i] = 7146908634.576 + i;
    }

  bd.x = _mm512_castpd_ps (c.x);
  if (memcmp(bd.a, c.a, 64))
    abort ();

  ad.x = _mm512_castpd_si512 (c.x);
  if (memcmp(ad.a, c.a, 64))
    abort ();

  cd.x = _mm512_castps_pd (b.x);
  if (memcmp(cd.a, b.a, 64))
    abort ();

  ad.x = _mm512_castps_si512 (b.x);
  if (memcmp(ad.a, b.a, 64))
    abort ();

  bd.x = _mm512_castsi512_ps (a.x);
  if (memcmp(bd.a, a.a, 64))
    abort ();

  cd.x = _mm512_castsi512_pd (a.x);
  if (memcmp(cd.a, a.a, 64))
    abort ();

  kd.x = _mm512_castpd512_pd128 (c.x);
  if (memcmp(kd.a, c.a, 16))
    abort ();

  hd.x = _mm512_castps512_ps128 (b.x);
  if (memcmp(hd.a, b.a, 16))
    abort ();

  gd.x = _mm512_castsi512_si128 (a.x);
  if (memcmp(gd.a, a.a, 16))
    abort ();

  fd.x = _mm512_castpd512_pd256 (c.x);
  if (memcmp(fd.a, c.a, 32))
    abort ();

  ed.x = _mm512_castps512_ps256 (b.x);
  if (memcmp(ed.a, b.a, 32))
    abort ();

  dd.x = _mm512_castsi512_si256 (a.x);
  if (memcmp(dd.a, a.a, 32))
    abort ();

  cd.x = _mm512_castpd128_pd512 (k.x);
  if (memcmp(cd.a, k.a, 16))
    abort ();

  bd.x = _mm512_castps128_ps512 (h.x);
  if (memcmp(bd.a, h.a, 16))
    abort ();

  ad.x = _mm512_castsi128_si512 (g.x);
  if (memcmp(ad.a, g.a, 16))
    abort ();

  cd.x = _mm512_castpd256_pd512 (f.x);
  if (memcmp(cd.a, f.a, 32))
    abort ();

  bd.x = _mm512_castps256_ps512 (e.x);
  if (memcmp(bd.a, e.a, 32))
    abort ();

  ad.x = _mm512_castsi256_si512 (d.x);
  if (memcmp(ad.a, d.a, 32))
    abort ();
}
