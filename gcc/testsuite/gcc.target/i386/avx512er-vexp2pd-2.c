/* { dg-do run } */
/* { dg-require-effective-target avx512er } */
/* { dg-options "-O2 -mavx512er" } */

#include "avx512er-check.h"
#include "avx512f-mask-type.h"
#include "avx512f-helper.h"
#include <math.h>

void static
compute_vexp2pd (double *s, double *r)
{
  int i;
  for (i = 0; i < 8; i++)
    r[i] = pow (2.0, s[i]);
}

void static
avx512er_test (void)
{
  union512d src, res1, res2, res3;
  __mmask8 mask = MASK_VALUE;
  double res_ref[8];
  int i;

  for (i = 0; i < 8; i++)
    {
      src.a[i] = 179.345 - 6.5645 * i;
      res2.a[i] = DEFAULT_VALUE;
    }

  res1.x = _mm512_exp2a23_pd (src.x);
  res2.x = _mm512_mask_exp2a23_pd (res2.x, mask, src.x);
  res3.x = _mm512_maskz_exp2a23_pd (mask, src.x);

  compute_vexp2pd (src.a, res_ref);

  if (check_rough_union512d (res1, res_ref, 0.0001))
    abort ();

  MASK_MERGE (d) (res_ref, mask, 8);
  if (check_rough_union512d (res2, res_ref, 0.0001))
    abort ();

  MASK_ZERO (d) (res_ref, mask, 8);
  if (check_rough_union512d (res3, res_ref, 0.0001))
    abort ();
}
