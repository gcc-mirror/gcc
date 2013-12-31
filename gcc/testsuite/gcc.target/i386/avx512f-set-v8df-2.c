/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

static __m512d
__attribute__ ((noinline))
foo (double x1, double x2, double x3, double x4,
     double x5, double x6, double x7, double x8)
{
  return _mm512_set_pd (x1, x2, x3, x4, x5, x6, x7, x8);
}

static __m512d
__attribute__ ((noinline))
foo_r (double x1, double x2, double x3, double x4,
       double x5, double x6, double x7, double x8)
{
  return _mm512_setr_pd (x8, x7, x6, x5, x4, x3, x2, x1);
}

static void
avx512f_test (void)
{
  double v[8] = { -3.3, 2.6, 1.48, 9.104, -23.9, -173.37, -13.48, 69.78 };
  union512d res;

  res.x = foo (v[7], v[6], v[5], v[4], v[3], v[2], v[1], v[0]);

  if (check_union512d (res, v))
    abort ();

  res.x = _mm512_setzero_pd ();

  res.x = foo_r (v[7], v[6], v[5], v[4], v[3], v[2], v[1], v[0]);

  if (check_union512d (res, v))
    abort ();
}
