/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"
#include "avx512f-helper.h"

static void
compute_vrcp14sd (double *s1, double *s2, double *r)
{
  r[0] = 1.0 / s1[0];
  r[1] = s2[1];
}

static void
avx512f_test (void)
{
  union128d s1, s2, res1, res2, res3;
  double res_ref[2];

  s1.x = _mm_set_pd (-3.0, 111.111);
  s2.x = _mm_set_pd (222.222, -2.0);
  res2.a[0] = DEFAULT_VALUE;

  res1.x = _mm_rcp14_sd (s1.x, s2.x);

  compute_vrcp14sd (s1.a, s2.a, res_ref);

  if (checkVd (res1.a, res_ref, 2))
    abort ();
}
