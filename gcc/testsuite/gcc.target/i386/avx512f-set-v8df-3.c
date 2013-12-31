/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

static __m512d
__attribute__ ((noinline))
foo (double x)
{
  return _mm512_set_pd (x, x, x, x, x, x, x, x);
}

static __m512d
__attribute__ ((noinline))
foo_r (double x)
{
  return _mm512_setr_pd (x, x, x, x, x, x, x, x);
}

static void
avx512f_test (void)
{
  int i;
  double e = 34.5;
  double v[8];
  union512d res;

  for (i = 0; i < 8; i++)
    v[i] = e;

  res.x = foo (e);

  if (check_union512d (res, v))
    abort ();

  res.x = _mm512_setzero_pd ();

  res.x = foo_r (e);

  if (check_union512d (res, v))
    abort ();
}
