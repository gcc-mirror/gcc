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
  union128d res1, res2, res3, src1, src2;
  MASK_TYPE mask = MASK_VALUE;
  double res_ref[SIZE];

  sign = -1;
  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = 1.5 + 34.67 * i * sign;
      src2.a[i] = -22.17 * i * sign + 1.0;
      sign = sign * -1;
    }
  for (i = 0; i < SIZE; i++)
      res2.a[i] = DEFAULT_VALUE;

  res1.x = _mm_max_round_sd (src1.x, src2.x, _MM_FROUND_NO_EXC);
  res2.x = _mm_mask_max_round_sd (res2.x, mask, src1.x, src2.x, _MM_FROUND_NO_EXC);
  res3.x = _mm_maskz_max_round_sd (mask, src1.x, src2.x, _MM_FROUND_NO_EXC);

  calc_max (res_ref, src1.a, src2.a);

  if (check_union128d (res1, res_ref))
    abort();
  
  MASK_MERGE (d) (res_ref, mask, 1);
  if (check_union128d (res2, res_ref))
    abort ();

  MASK_ZERO (d) (res_ref, mask, 1);
  if (check_union128d (res3, res_ref))
    abort ();
}

