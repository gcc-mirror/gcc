/* { dg-do run } */
/* { dg-options "-mavx512f -O2 -std=c99" } */
/* { dg-require-effective-target avx512f } */
/* { dg-require-effective-target c99_runtime } */

#include "avx512f-check.h"
#include <math.h>
#include "avx512f-helper.h"

#define SIZE (128/32)
#include "avx512f-mask-type.h"

union fp_int_t
{
  int int_val;
  float fp_val;
};

float
get_norm_mant (float source, int signctrl, int interv)
{
  int src, sign, exp, fraction;
  union fp_int_t bin_conv;

  bin_conv.fp_val = source;
  src = bin_conv.int_val;
  sign = (signctrl & 0x1) ? 0 : (src >> 31);
  exp = (src & 0x7f800000) >> 23;
  fraction = (src & 0x7fffff);

  if (isnan (source))
    return signbit (source) ? -NAN : NAN;
  if (source == 0.0 || source == -0.0 || isinf (source))
    return sign ? -1.0 : 1.0;
  if (signbit (source) && (signctrl & 0x2))
    return -NAN;
  if (!isnormal (source))
    {
      src = (src & 0xffbfffff);
      exp = 0x7f;
      while (!(src & 0x400000))
	{
	  src += fraction & 0x400000;
	  fraction = fraction << 1;
	  exp--;
	}
    }

  switch (interv)
    {
    case 0:
      exp = 0x7f;
      break;
    case 1:
      exp = ((exp - 0x7f) & 0x1) ? 0x7e : 0x7f;
      break;
    case 2:
      exp = 0x7e;
      break;
    case 3:
      exp = (fraction & 0x400000) ? 0x7e : 0x7f;
      break;
    default:
      abort ();
    }

  bin_conv.int_val = (sign << 31) | (exp << 23) | fraction;

  return bin_conv.fp_val;

}

static void
compute_vgetmantss (float *r, float *s1, float *s2, int interv,
		    int signctrl)
{
  int i;
  r[0] = get_norm_mant (s2[0], signctrl, interv);
  for (i = 1; i < SIZE; i++)
    {
      r[i] = s1[i];
    }
}

static void
avx512f_test (void)
{
  union128 res1, res2, res3, res4, res5, res6, src1, src2;
  float res_ref[4];
  MASK_TYPE mask = MASK_VALUE;

  src1.x = _mm_set_ps (-24.043, 68.346, -43.35, 546.46);
  src2.x = _mm_set_ps (222.222, 333.333, 444.444, -2.0);

  int i; 
  for (i = 0; i < SIZE; i++)
    {
      res2.a[i] = DEFAULT_VALUE;
      res5.a[i] = DEFAULT_VALUE;
    }

  res1.x = _mm_getmant_ss (src1.x, src2.x, _MM_MANT_NORM_p5_1, _MM_MANT_SIGN_src);
  res2.x = _mm_mask_getmant_ss (res2.x, mask, src1.x, src2.x, _MM_MANT_NORM_p5_1, _MM_MANT_SIGN_src);
  res3.x = _mm_maskz_getmant_ss (mask, src1.x, src2.x, _MM_MANT_NORM_p5_1, _MM_MANT_SIGN_src);
  res4.x = _mm_getmant_round_ss (src1.x, src2.x, _MM_MANT_NORM_p5_1, _MM_MANT_SIGN_src, _MM_FROUND_NO_EXC);
  res5.x = _mm_mask_getmant_round_ss (res5.x, mask, src1.x, src2.x, _MM_MANT_NORM_p5_1, _MM_MANT_SIGN_src, _MM_FROUND_NO_EXC);
  res6.x = _mm_maskz_getmant_round_ss (mask, src1.x, src2.x, _MM_MANT_NORM_p5_1, _MM_MANT_SIGN_src, _MM_FROUND_NO_EXC);

  compute_vgetmantss (res_ref, src1.a, src2.a, _MM_MANT_NORM_p5_1, _MM_MANT_SIGN_src);

  if (check_union128 (res1, res_ref))
    abort ();
  
  MASK_MERGE () (res_ref, mask, 1);
  if (check_union128 (res2, res_ref))
    abort ();

  MASK_ZERO () (res_ref, mask, 1);
  if (check_union128 (res3, res_ref))
    abort ();

  compute_vgetmantss (res_ref, src1.a, src2.a, _MM_MANT_NORM_p5_1, _MM_MANT_SIGN_src);

  if (check_union128 (res4, res_ref))
    abort ();
  
  MASK_MERGE () (res_ref, mask, 1);
  if (check_union128 (res5, res_ref))
    abort ();

  MASK_ZERO () (res_ref, mask, 1);
  if (check_union128 (res6, res_ref))
    abort ();
}
