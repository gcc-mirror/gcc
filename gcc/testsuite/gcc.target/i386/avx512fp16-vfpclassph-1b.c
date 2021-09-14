/* { dg-do run } */
/* { dg-options "-O2 -mavx512fp16" } */
/* { dg-require-effective-target avx512fp16 } */

#define AVX512FP16
#include "avx512f-helper.h"

#include <math.h>
#include <limits.h>
#include <float.h>
#include "avx512f-mask-type.h"
#define SIZE (AVX512F_LEN / 16)

#ifndef __FPCLASSPH__
#define __FPCLASSPH__
int check_fp_class_hp (_Float16 src, int imm)
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

MASK_TYPE
CALC (_Float16 *s1, int imm)
{
  int i;
  MASK_TYPE res = 0;

  for (i = 0; i < SIZE; i++)
    if (check_fp_class_hp(s1[i], imm))
      res = res | (1 << i);

  return res;
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, h) src;
  MASK_TYPE res1, res2, res_ref = 0;
  MASK_TYPE mask = MASK_VALUE;

  src.a[0] = NAN;
  src.a[1] = 1.0 / 0.0;
  for (i = 1; i < SIZE; i++)
    {
      src.a[i] = -24.43 + 0.6 * i;
    }

  res1 = INTRINSIC (_fpclass_ph_mask) (src.x, 0xFF);
  res2 = INTRINSIC (_mask_fpclass_ph_mask) (mask, src.x, 0xFF);

  res_ref = CALC (src.a, 0xFF);

  if (res_ref != res1)
    abort ();

  if ((mask & res_ref) != res2)
    abort ();
}
