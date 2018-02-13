/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include <math.h>
#include "avx512f-check.h"

#define SIZE (128 / 32)
#include "avx512f-mask-type.h"

static void
compute_scalefss (float *s1, float *s2, float *r)
{
  r[0] = s1[0] * (float) pow (2, floor (s2[0]));
  r[1] = s1[1];
  r[2] = s1[2];
  r[3] = s1[3];
}

static void
avx512f_test (void)
{
  union128 res1, res2, res3, res4;
  union128 s1, s2;
  float res_ref[SIZE];
  MASK_TYPE mask = MASK_VALUE;
  int i;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = 11.5 * (i + 1);
      s2.a[i] = 10.5 * (i + 1);
      res_ref[i] = 9.5 * (i + 1);
      res1.a[i] = DEFAULT_VALUE;
      res2.a[i] = DEFAULT_VALUE;
      res3.a[i] = DEFAULT_VALUE;
      res4.a[i] = DEFAULT_VALUE;
    }

  res1.x = _mm_scalef_ss (s1.x, s2.x);
  res2.x = _mm_scalef_round_ss (s1.x, s2.x,
              _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  res3.x = _mm_mask_scalef_round_ss (s1.x, mask, s1.x, s2.x,
              _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  res4.x = _mm_maskz_scalef_round_ss (mask, s1.x, s2.x,
              _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);

  compute_scalefss (s1.a, s2.a, res_ref);

  if (check_union128 (res1, res_ref))
    abort ();
  if (check_union128 (res2, res_ref))                                         
    abort ();

  MASK_MERGE () (res_ref, mask, 1);

  if (check_union128 (res3, res_ref))
    abort ();

  MASK_ZERO () (res_ref, mask, 1);

  if (check_union128 (res4, res_ref))
    abort ();
}
