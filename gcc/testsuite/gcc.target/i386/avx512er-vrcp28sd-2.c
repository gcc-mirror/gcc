/* { dg-do run } */
/* { dg-require-effective-target avx512er } */
/* { dg-options "-O2 -mavx512er" } */
/* { dg-warning "AVX512ER support will be removed in GCC 15" "" { target *-*-* } 0 } */

#include "avx512er-check.h"
#include "avx512f-mask-type.h"
#include "avx512f-helper.h"
#include <math.h>

#define IMM 0x23

void static
avx512er_test (void)
{
  union128d src1, src2, res, res1, res2, res3, res4;
  double res_ref[2];
  MASK_TYPE mask = MASK_VALUE;
  int i;
  
  for (i = 0; i < 2; i++)
    {
      src1.a[i] = 179.345 - 6.5645 * i;
      src2.a[i] = 204179.345 + 6.5645 * i;
      res_ref[i] = src1.a[i];
    }

  res_ref[0] = 1.0 / src2.a[0];

  res.x = _mm_rcp28_round_sd (src1.x, src2.x, _MM_FROUND_NO_EXC);
  res1.x = _mm_mask_rcp28_sd (src1.x, IMM, src1.x, src2.x);
  res2.x = _mm_mask_rcp28_round_sd (src1.x, IMM, src1.x, src2.x,
				    _MM_FROUND_TO_NEAREST_INT
				    | _MM_FROUND_NO_EXC);
  res3.x = _mm_maskz_rcp28_sd (IMM, src1.x, src2.x);
  res4.x = _mm_maskz_rcp28_round_sd (IMM, src1.x, src2.x,
				     _MM_FROUND_TO_NEAREST_INT
				     | _MM_FROUND_NO_EXC);


  if (checkVd (res.a, res_ref, 2))
    abort ();

  MASK_MERGE (d) (res_ref, mask, 1);

  if (checkVd (res1.a, res_ref, 2))
    abort ();

  if (checkVd (res2.a, res_ref, 2))
    abort ();

  MASK_ZERO (d) (res_ref, mask, 1);

  if (checkVd (res3.a, res_ref, 2))
    abort ();

  if (checkVd (res4.a, res_ref, 2))
    abort ();
}
