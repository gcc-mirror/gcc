/* { dg-do run } */
/* { dg-require-effective-target avx512er } */
/* { dg-options "-O2 -mavx512er" } */
/* { dg-warning "AVX512ER support will be removed in GCC 15" "" { target *-*-* } 0 } */

#include "avx512er-check.h"
#include "avx512f-mask-type.h"
#include "avx512f-helper.h"
#include <math.h>

void static
compute_vexp2ps (float *s, float *r)
{
  int i;
  for (i = 0; i < 16; i++)
    r[i] = pow (2.0, s[i]);
}

void static
avx512er_test (void)
{
  union512 src, res1, res2, res3;
  __mmask16 mask = MASK_VALUE;
  float res_ref[16];
  int i;

  for (i = 0; i < 16; i++)
    {
      src.a[i] = 79.345 - 6.5645 * i;
      res2.a[i] = DEFAULT_VALUE;
    }

  res1.x = _mm512_exp2a23_ps (src.x);
  res2.x = _mm512_mask_exp2a23_ps (res2.x, mask, src.x);
  res3.x = _mm512_maskz_exp2a23_ps (mask, src.x);

  compute_vexp2ps (src.a, res_ref);

  if (check_rough_union512 (res1, res_ref, 0.0001))
    abort ();

  MASK_MERGE ()(res_ref, mask, 16);
  if (check_rough_union512 (res2, res_ref, 0.0001))
    abort ();

  MASK_ZERO ()(res_ref, mask, 16);
  if (check_rough_union512 (res3, res_ref, 0.0001))
    abort ();
}
