/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"
#include "avx512f-helper.h"

static void
compute_vrcp14ss (float *s1, float *s2, float *r)
{
  r[0] = 1.0 / s2[0];
  r[1] = s1[1];
  r[2] = s1[2];
  r[3] = s1[3];
}

static void
avx512f_test (void)
{
  union128 s1, s2, res1, res2, res3;
  float res_ref[4];
  __mmask8 m = 0;

  s1.x = _mm_set_ps (-24.043, 68.346, -43.35, 546.46);
  s2.x = _mm_set_ps (222.222, 333.333, 444.444, -2.0);

  res1.x = _mm_rcp14_ss (s1.x, s2.x); 

  compute_vrcp14ss (s1.a, s2.a, res_ref);

  if (checkVf (res1.a, res_ref, 4))
    abort ();

  res2.x = _mm_set_ps (5.0, 6.0, 7.0, DEFAULT_VALUE);
  res2.x = _mm_mask_rcp14_ss(res2.x, m, s1.x, s2.x);

  MASK_MERGE () (res_ref, m, 1);
  if (checkVf (res2.a, res_ref, 4))
    abort();

  res3.x = _mm_maskz_rcp14_ss(m, s1.x, s2.x);
  
  MASK_ZERO () (res_ref, m, 1);
  if (checkVf (res3.a, res_ref, 4))
    abort();

}
