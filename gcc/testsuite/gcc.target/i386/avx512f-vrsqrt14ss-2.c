/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include <math.h>
#include "avx512f-check.h"
#include "avx512f-helper.h"

static void
compute_vrsqrt14ss (float *s1, float *s2, float *r)
{
  r[0] = 1.0 / sqrt (s2[0]);
  r[1] = s1[1];
  r[2] = s1[2];
  r[3] = s1[3];
}

static void
avx512f_test (void)
{
  union128 s1, s2, res1, res2, res3;
  __mmask8 m = 0;
  float res_ref[4];

  s1.x = _mm_set_ps (-24.43, 68.346, -43.35, 546.46);
  s2.x = _mm_set_ps (222.222, 333.333, 444.444, 4.0);

  res1.x = _mm_rsqrt14_ss (s1.x, s2.x);

  compute_vrsqrt14ss (s1.a, s2.a, res_ref);

  if (check_fp_union128 (res1, res_ref))
    abort ();

  res2.x = _mm_set_ps (5.0, 6.0, 7.0, DEFAULT_VALUE);
  res2.x = _mm_mask_rsqrt14_ss(res2.x, m, s1.x, s2.x);

  MASK_MERGE () (res_ref, m, 1);
  if (checkVf (res2.a, res_ref, 4))
    abort();

  res3.x = _mm_maskz_rsqrt14_ss(m, s1.x, s2.x);
  
  MASK_ZERO () (res_ref, m, 1);
  if (checkVf (res3.a, res_ref, 4))
    abort();
}
