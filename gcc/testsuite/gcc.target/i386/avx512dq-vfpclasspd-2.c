/* { dg-do run } */
/* { dg-options "-O2 -mavx512dq" } */
/* { dg-require-effective-target avx512dq } */

#define AVX512DQ
#include "avx512f-helper.h"

#include <math.h>
#include <limits.h>
#include <float.h>
#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

#ifndef __FPCLASSPD__
#define __FPCLASSPD__
int check_fp_class_dp (double src, int imm)
{
  int qNaN_res = isnan (src);
  int sNaN_res = isnan (src);
  int Pzero_res = (src == 0.0);
  int Nzero_res = (src == -0.0);
  int PInf_res = (isinf (src) == 1);
  int NInf_res = (isinf (src) == -1);
  int Denorm_res = (fpclassify (src) == FP_SUBNORMAL);
  int FinNeg_res = __builtin_finite (src) && (src < 0);

  int result = (((imm & 1) && qNaN_res)
		|| (((imm >> 1) & 1) && Pzero_res)
		|| (((imm >> 2) & 1) && Nzero_res)
		|| (((imm >> 3) & 1) && PInf_res)
		|| (((imm >> 4) & 1) && NInf_res)
		|| (((imm >> 5) & 1) && Denorm_res)
		|| (((imm >> 6) & 1) && FinNeg_res)
		|| (((imm >> 7) & 1) && sNaN_res));
  return result;
}
#endif

__mmask8
CALC (double *s1, int imm)
{
  int i;
  __mmask8 res = 0;

  for (i = 0; i < SIZE; i++)
    if (check_fp_class_dp(s1[i], imm))
      res = res | (1 << i);

  return res;
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, d) src;
  __mmask8 res1, res2, res_ref = 0;
  MASK_TYPE mask = MASK_VALUE;

  src.a[0] = NAN;
  src.a[1] = 1.0 / 0.0;
  for (i = 2; i < SIZE; i++)
    {
      src.a[i] = -24.43 + 0.6 * i;
    }

  res1 = INTRINSIC (_fpclass_pd_mask) (src.x, 0xFF);
  res2 = INTRINSIC (_mask_fpclass_pd_mask) (mask, src.x, 0xFF);

  res_ref = CALC (src.a, 0xFF);

  if (res_ref != res1)
    abort ();

  if ((res_ref & mask) != res2)
    abort ();
}
