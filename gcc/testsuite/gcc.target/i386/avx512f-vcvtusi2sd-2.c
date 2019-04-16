/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

static void
  __attribute__ ((noinline, unused))
compute_vcvtusi2sd (double *s1, unsigned s2, double *r)
{
  r[0] = (double) s2;
  r[1] = s1[1];
}

static void
avx512f_test (void)
{
  union128d s1, res;
  unsigned s2;
  double res_ref[2];

  s1.x = _mm_set_pd (-24.43, -43.35);
  s2 = 0xFEDCA987;

  asm volatile ("" : "+m" (s2));
  res.x = _mm_cvtu32_sd (s1.x, s2);
  asm volatile ("" : "+m" (s2));

  compute_vcvtusi2sd (s1.a, s2, res_ref);

  if (check_union128d (res, res_ref))
    abort ();
}
