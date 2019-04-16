/* { dg-do run { target { ! ia32 } } } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

static void
  __attribute__ ((noinline, unused))
compute_vcvtusi2ss (float *s1, unsigned long long s2, float *r)
{
  r[0] = (float) s2;
  r[1] = s1[1];
  r[2] = s1[2];
  r[3] = s1[3];
}

static void
avx512f_test (void)
{
  union128 s1, res;
  unsigned long long s2;
  float res_ref[4];

  s1.x = _mm_set_ps (-24.43, 68.346, -43.35, 546.46);
  s2 = 0xFEDCBA9876543210;

  asm volatile ("" : "+m" (s2));
  res.x = _mm_cvtu64_ss (s1.x, s2);
  asm volatile ("" : "+m" (s2));

  compute_vcvtusi2ss (s1.a, s2, res_ref);

  if (check_union128 (res, res_ref))
    abort ();
}
