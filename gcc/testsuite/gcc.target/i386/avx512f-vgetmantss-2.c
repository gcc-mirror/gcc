/* { dg-do run } */
/* { dg-options "-mavx512f -O2 -std=c99" } */
/* { dg-require-effective-target avx512f } */
/* { dg-require-effective-target c99_runtime } */

#include "avx512f-check.h"
#include "avx512f-helper.h"
#include <math.h>

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
  for (i = 1; i < 4; i++)
    {
      r[i] = s1[i];
    }
}

static void
avx512f_test (void)
{
  int i, sign;
  union128 res1, src1, src2;
  float res_ref[4];
  int interv = _MM_MANT_NORM_p5_1;
  int signctrl = _MM_MANT_SIGN_src;

  src1.x = _mm_set_ps (-24.043, 68.346, -43.35, 546.46);
  src2.x = _mm_set_ps (222.222, 333.333, 444.444, -2.0);

  res1.x = _mm_getmant_ss (src1.x, src2.x, interv, signctrl);

  compute_vgetmantss (res_ref, src1.a, src2.a, interv, signctrl);

  if (check_union128 (res1, res_ref))
    abort ();
}
