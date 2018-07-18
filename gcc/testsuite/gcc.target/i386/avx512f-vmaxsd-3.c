/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

#include "avx512f-helper.h"

#define SIZE (128 / 64)
#include "avx512f-mask-type.h"

static void
calc_max (double *r, double *s1, double *s2)
{
  r[0] = s1[0] > s2[0] ? s1[0] : s2[0];
  r[1] = s1[1];
}

void
avx512f_test (void)
{
  int i, sign;
  union128d res1, res2, res3, res4, src1, src2;
  MASK_TYPE mask = 0;
  double res_ref[SIZE];

  sign = -1;
  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = 1.5 + 34.67 * i * sign;
      src2.a[i] = -22.17 * i * sign + 1.0;
      res1.a[i] = DEFAULT_VALUE;
      res3.a[i] = DEFAULT_VALUE;
      sign = sign * -1;
    }

  res1.x = _mm_mask_max_sd (res1.x, mask, src1.x, src2.x);
  res2.x = _mm_maskz_max_sd (mask, src1.x, src2.x);
  res3.x = _mm_mask_max_round_sd (res3.x, mask, src1.x, src2.x, _MM_FROUND_NO_EXC);
  res4.x = _mm_maskz_max_round_sd (mask, src1.x, src2.x, _MM_FROUND_NO_EXC);

  calc_max (res_ref, src1.a, src2.a);

  MASK_MERGE (d) (res_ref, mask, 1);
  if (check_union128d (res1, res_ref))
    abort ();

  MASK_ZERO (d) (res_ref, mask, 1);
  if (check_union128d (res2, res_ref))
    abort ();

  calc_max (res_ref, src1.a, src2.a);

  MASK_MERGE (d) (res_ref, mask, 1);
  if (check_union128d (res3, res_ref))
    abort ();

  MASK_ZERO (d) (res_ref, mask, 1);
  if (check_union128d (res4, res_ref))
    abort ();
}

