/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#define SIZE (128 / 32)

#include <math.h>
#include "avx512f-check.h"
#include "avx512f-helper.h"
#include "avx512f-mask-type.h"

static void
compute_vgetexpss (float *s, float *r)
{
  int i;
  r[0] = floor (log (s[0]) / log (2));
  for(i = 1; i < SIZE; i++)
     r[i] = s[i];
}

void static
avx512f_test (void)
{
  int i;
  union128 res1, res2, res3, res4, res5, res6, s1;
  MASK_TYPE mask = MASK_VALUE;
  float res_ref[SIZE];

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = 5.0 - i;
      res2.a[i] = DEFAULT_VALUE;
      res5.a[i] = DEFAULT_VALUE;
    }

  res1.x = _mm_getexp_ss (s1.x, s1.x);
  res2.x = _mm_mask_getexp_ss (res2.x, mask, s1.x, s1.x);
  res3.x = _mm_maskz_getexp_ss (mask, s1.x, s1.x);
  res4.x = _mm_getexp_round_ss (s1.x, s1.x, _MM_FROUND_NO_EXC);
  res5.x = _mm_mask_getexp_round_ss (res5.x, mask, s1.x, s1.x, _MM_FROUND_NO_EXC);
  res6.x = _mm_maskz_getexp_round_ss (mask, s1.x, s1.x, _MM_FROUND_NO_EXC);

  compute_vgetexpss (s1.a, res_ref);

  if (check_fp_union128 (res1, res_ref))
    abort ();

  MASK_MERGE () (res_ref, mask, 1);
  if (check_fp_union128 (res2, res_ref))
    abort ();

  MASK_ZERO () (res_ref, mask, 1);
  if (check_fp_union128 (res3, res_ref))
    abort ();

  compute_vgetexpss (s1.a, res_ref);

  if (check_fp_union128 (res4, res_ref))
    abort ();

  MASK_MERGE () (res_ref, mask, 1);
  if (check_fp_union128 (res5, res_ref))
    abort ();

  MASK_ZERO () (res_ref, mask, 1);
  if (check_fp_union128 (res6, res_ref))
    abort ();
}
