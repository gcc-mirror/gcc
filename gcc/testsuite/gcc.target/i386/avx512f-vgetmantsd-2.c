/* { dg-do run } */
/* { dg-options "-mavx512f -O2 -std=c99" } */
/* { dg-require-effective-target avx512f } */
/* { dg-require-effective-target c99_runtime } */

#include "avx512f-check.h"
#include <math.h>
#include "avx512f-helper.h"

#define SIZE (128/64)
#include "avx512f-mask-type.h"

union fp_int_t
{
  long long int int_val;
  double fp_val;
};

double
get_norm_mant (double source, int signctrl, int interv)
{
  long long src, sign, exp, fraction;

  union fp_int_t bin_conv;

  bin_conv.fp_val = source;
  src = bin_conv.int_val;
  sign = (signctrl & 0x1) ? 0 : (src >> 63);
  exp = (src & 0x7ff0000000000000) >> 52;
  fraction = (src & 0xfffffffffffff);

  if (isnan (source))
    return signbit (source) ? -NAN : NAN;
  if (source == 0.0 || source == -0.0 || isinf (source))
    return sign ? -1.0 : 1.0;
  if (signbit (source) && (signctrl & 0x2))
    return -NAN;
  if (!isnormal (source))
    {
      src = (src & 0xfff7ffffffffffff);
      exp = 0x3ff;
      while (!(src & 0x8000000000000))
	{
	  src += fraction & 0x8000000000000;
	  fraction = fraction << 1;
	  exp--;
	}
    }

  switch (interv)
    {
    case 0:
      exp = 0x3ff;
      break;
    case 1:
      exp = ((exp - 0x3ff) & 0x1) ? 0x3fe : 0x3ff;
      break;
    case 2:
      exp = 0x3fe;
      break;
    case 3:
      exp = (fraction & 0x8000000000000) ? 0x3fe : 0x3ff;
      break;
    default:
      abort ();
    }

  bin_conv.int_val = (sign << 63) | (exp << 52) | fraction;
  return bin_conv.fp_val;
}

static void
compute_vgetmantsd (double *r, double *s1, double *s2, int interv,
		    int signctrl)
{
  r[0] = get_norm_mant (s2[0], signctrl, interv);
  r[1] = s1[1];
}

static void
avx512f_test (void)
{
  union128d res1, res2, res3, res4, res5, res6, src1, src2;
  double res_ref[2];
  MASK_TYPE mask = MASK_VALUE;

  src1.x = _mm_set_pd (-3.0, 111.111);
  src2.x = _mm_set_pd (222.222, -2.0);
  
  int i; 
  for (i = 0; i < SIZE; i++)
    {
      res2.a[i] = DEFAULT_VALUE;
      res5.a[i] = DEFAULT_VALUE;
    }

  res1.x = _mm_getmant_sd (src1.x, src2.x, _MM_MANT_NORM_p5_1, _MM_MANT_SIGN_src);
  res2.x = _mm_mask_getmant_sd (res2.x, mask, src1.x, src2.x, _MM_MANT_NORM_p5_1, _MM_MANT_SIGN_src);
  res3.x = _mm_maskz_getmant_sd (mask, src1.x, src2.x, _MM_MANT_NORM_p5_1, _MM_MANT_SIGN_src);
  res4.x = _mm_getmant_round_sd (src1.x, src2.x, _MM_MANT_NORM_p5_1, _MM_MANT_SIGN_src, _MM_FROUND_NO_EXC);
  res5.x = _mm_mask_getmant_round_sd (res5.x, mask, src1.x, src2.x, _MM_MANT_NORM_p5_1, _MM_MANT_SIGN_src, _MM_FROUND_NO_EXC);
  res6.x = _mm_maskz_getmant_round_sd (mask, src1.x, src2.x, _MM_MANT_NORM_p5_1, _MM_MANT_SIGN_src, _MM_FROUND_NO_EXC);

  compute_vgetmantsd (res_ref, src1.a, src2.a, _MM_MANT_NORM_p5_1, _MM_MANT_SIGN_src);

  if (check_union128d (res1, res_ref))
    abort ();
  
  MASK_MERGE (d) (res_ref, mask, 1);
  if (check_union128d (res2, res_ref))
    abort ();

  MASK_ZERO (d) (res_ref, mask, 1);
  if (check_union128d (res3, res_ref))
    abort ();

  compute_vgetmantsd (res_ref, src1.a, src2.a, _MM_MANT_NORM_p5_1, _MM_MANT_SIGN_src);

  if (check_union128d (res4, res_ref))
    abort ();
  
  MASK_MERGE (d) (res_ref, mask, 1);
  if (check_union128d (res5, res_ref))
    abort ();

  MASK_ZERO (d) (res_ref, mask, 1);
  if (check_union128d (res6, res_ref))
    abort ();

}
