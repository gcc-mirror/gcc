/* { dg-do run } */
/* { dg-require-effective-target avx512er } */
/* { dg-options "-O2 -mavx512er" } */
/* { dg-warning "AVX512ER support will be removed in GCC 15" "" { target *-*-* } 0 } */

#include "avx512er-check.h"
#include "avx512f-mask-type.h"
#include "avx512f-helper.h"
#include <math.h>

void static
compute_vrsqrt28pd (double *s, double *r)
{
  int i;
  for (i = 0; i < 8; i++)
    r[i] = 1.0 / sqrt (s[i]);
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

  res1.x = _mm512_rsqrt28_pd (src.x);
  res2.x = _mm512_mask_rsqrt28_pd (res2.x, mask, src.x);
  res3.x = _mm512_maskz_rsqrt28_pd (mask, src.x);

  compute_vrsqrt28pd (src.a, res_ref);

  if (check_rough_union512d (res1, res_ref, 0.0001))
    abort ();

  MASK_MERGE (d) (res_ref, mask, 8);
  if (check_rough_union512d (res2, res_ref, 0.0001))
    abort ();

  MASK_ZERO (d) (res_ref, mask, 8);
  if (check_rough_union512d (res3, res_ref, 0.0001))
    abort ();
}
