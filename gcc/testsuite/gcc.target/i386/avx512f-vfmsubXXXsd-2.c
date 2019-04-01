/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

#include "avx512f-helper.h"

#define SIZE (128 / 64)
#include "avx512f-mask-type.h"

static void
calc_add (double *s1, double *s2, double *s3, double* r)
{
  r[0] = s1[0] * s2[0] - s3[0];
  
  int i;
  for (i = 1; i < SIZE; i++)
    r[i] = s1[i];
}

static void
calc_add_3 (double *s1, double *s2, double *s3, double* r)
{
  r[0] = s2[0] * s3[0] - s1[0];
  
  int i;
  for (i = 1; i < SIZE; i++)
    r[i] = s1[i];
}

void
avx512f_test (void)
{
  int i, sign;
  union128d res1, res2, res3, res4, res5, res6, res7, src1, src2, src3;
  MASK_TYPE mask = MASK_VALUE;
  double res_ref1[SIZE], res_ref2[SIZE];

  sign = -1;
  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = DEFAULT_VALUE;
      src2.a[i] = 56.78 * (i + 1) * sign;
      src3.a[i] = 90.12 * (i + 2) * sign;
      sign = sign * -1;
    }
  for (i = 0; i < SIZE; i++)
    {
      res1.a[i] = DEFAULT_VALUE;
      res2.a[i] = DEFAULT_VALUE;
      res5.a[i] = DEFAULT_VALUE;
      res6.a[i] = DEFAULT_VALUE;
    }

  calc_add (src1.a, src2.a, src3.a, res_ref1);
  calc_add_3(src1.a, src2.a, src3.a, res_ref2);

  res1.x = _mm_mask_fmsub_sd (src1.x, mask, src2.x, src3.x);
  res2.x = _mm_mask3_fmsub_sd (src2.x, src3.x, src1.x, mask);
  res3.x = _mm_maskz_fmsub_sd (mask, src1.x, src2.x, src3.x);
  res4.x = _mm_fmsub_round_sd (src1.x, src2.x, src3.x, _MM_FROUND_NO_EXC);
  res5.x = _mm_mask_fmsub_round_sd (src1.x, mask, src2.x, src3.x, _MM_FROUND_NO_EXC);
  res6.x = _mm_mask3_fmsub_round_sd (src2.x, src3.x, src1.x, mask, _MM_FROUND_NO_EXC);
  res7.x = _mm_maskz_fmsub_round_sd (mask, src1.x, src2.x, src3.x, _MM_FROUND_NO_EXC);

  if (check_union128d (res4, res_ref1))
    abort();

  MASK_ZERO (d) (res_ref1, mask, 1);
  if (check_union128d (res3, res_ref1))
    abort ();

  MASK_ZERO (d) (res_ref1, mask, 1);
  if (check_union128d (res7, res_ref1))
    abort ();

  MASK_MERGE (d) (res_ref2, mask, 1);
  if (check_union128d (res2, res_ref2))
    abort ();

  MASK_MERGE (d) (res_ref2, mask, 1);
  if (check_union128d (res6, res_ref2))
    abort ();

  MASK_MERGE (d) (res_ref1, mask, 1);
  if (check_union128d (res1, res_ref1))
    abort ();
  
  MASK_MERGE (d) (res_ref1, mask, 1);
  if (check_union128d (res5, res_ref1))
    abort ();
}

