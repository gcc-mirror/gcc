/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-Ofast -mavx -mno-avx2" } */

#ifndef CHECK_H
#define CHECK_H "avx-check.h"
#define TEST avx_test
#define SRC "avx-vround-1.c"
#endif

#include CHECK_H
#include SRC

static void
TEST (void)
{
  union128d a, ae;
  union128 b, be;
  union256d c, ce;
  union256 d, de;
  if (f1 (0.5) != 1.0 || f1 (1.5) != 2.0 || f1 (-0.5) != -1.0 || f1 (-1.5) != -2.0)
    abort ();
  if (f2 (0.5f) != 1.0f || f2 (1.5f) != 2.0f || f2 (-0.5f) != -1.0f || f2 (-1.5f) != -2.0f)
    abort ();
  a.x = f3 (_mm_set1_pd (7.0), _mm_set1_pd (0.5));
  ae.x = _mm_set_pd (7.0, 0.0);
  if (check_union128d (a, ae.a))
    abort ();
  a.x = f3 (_mm_set1_pd (7.0), _mm_set1_pd (1.5));
  ae.x = _mm_set_pd (7.0, 2.0);
  if (check_union128d (a, ae.a))
    abort ();
  a.x = f3 (_mm_set1_pd (7.0), _mm_set1_pd (-0.5));
  ae.x = _mm_set_pd (7.0, 0.0);
  if (check_union128d (a, ae.a))
    abort ();
  a.x = f3 (_mm_set1_pd (7.0), _mm_set1_pd (-1.5));
  ae.x = _mm_set_pd (7.0, -2.0);
  if (check_union128d (a, ae.a))
    abort ();
  b.x = f4 (_mm_set1_ps (7.0f), _mm_set1_ps (0.5f));
  be.x = _mm_set_ps (7.0f, 7.0f, 7.0f, 0.0f);
  if (check_union128 (b, be.a))
    abort ();
  b.x = f4 (_mm_set1_ps (7.0f), _mm_set1_ps (1.5f));
  be.x = _mm_set_ps (7.0f, 7.0f, 7.0f, 2.0f);
  if (check_union128 (b, be.a))
    abort ();
  b.x = f4 (_mm_set1_ps (7.0f), _mm_set1_ps (-0.5f));
  be.x = _mm_set_ps (7.0f, 7.0f, 7.0f, 0.0f);
  if (check_union128 (b, be.a))
    abort ();
  b.x = f4 (_mm_set1_ps (7.0f), _mm_set1_ps (-1.5f));
  be.x = _mm_set_ps (7.0f, 7.0f, 7.0f, -2.0f);
  if (check_union128 (b, be.a))
    abort ();
  a.x = f5 (_mm_set_pd (0.5, 1.5));
  ae.x = _mm_set_pd (0.0, 2.0);
  if (check_union128d (a, ae.a))
    abort ();
  a.x = f5 (_mm_set_pd (-0.5, -1.5));
  ae.x = _mm_set_pd (0.0, -2.0);
  if (check_union128d (a, ae.a))
    abort ();
  b.x = f6 (_mm_set_ps (0.5f, 1.5f, -0.5f, -1.5f));
  be.x = _mm_set_ps (0.0f, 2.0f, 0.0f, -2.0f);
  if (check_union128 (b, be.a))
    abort ();
  c.x = f7 (_mm256_set_pd (0.5, 1.5, -0.5, -1.5));
  ce.x = _mm256_set_pd (0.0, 2.0, 0.0, -2.0);
  if (check_union256d (c, ce.a))
    abort ();
  d.x = f8 (_mm256_set_ps (0.5f, 1.5f, -0.5f, -1.5f, 0.25f, 1.0f, -16.5f, 0.75f));
  de.x = _mm256_set_ps (0.0f, 2.0f, 0.0f, -2.0f, 0.0f, 1.0f, -16.0f, 1.0f);
  if (check_union256 (d, de.a))
    abort ();
}
