/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include <math.h>
#include "avx512f-check.h"

#define SIZE (128 / 64)
#include "avx512f-mask-type.h"

static void
compute_sqrtsd (double *s1, double *s2, double *r)
{
  r[0] = sqrt(s2[0]);
  r[1] = s1[1];
}

void static
avx512f_test (void)
{
  union128d res1, res2, res3;
  union128d s1, s2;
  double res_ref[SIZE];
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
    }

  res1.x = _mm_sqrt_round_sd (s1.x, s2.x, 
                _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  res2.x = _mm_mask_sqrt_round_sd (s1.x, mask, s1.x, s2.x,
		_MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  res3.x = _mm_maskz_sqrt_round_sd (mask, s1.x, s2.x,
		_MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);

  compute_sqrtsd (s1.a, s2.a, res_ref);

  if (check_union128d (res1, res_ref))
    abort ();

  MASK_MERGE (d) (res_ref, mask, 1);

  if (check_union128d (res2, res_ref))
    abort ();

  MASK_ZERO (d) (res_ref, mask, 1);

  if (check_union128d (res3, res_ref))
    abort ();
}




