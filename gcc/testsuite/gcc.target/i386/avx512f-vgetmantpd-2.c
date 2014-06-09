/* { dg-do run } */
/* { dg-options "-O2 -mavx512f -std=c99" } */
/* { dg-require-effective-target avx512f } */
/* { dg-require-effective-target c99_runtime } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"
#include <math.h>

#ifndef GET_NORM_MANT
#define GET_NORM_MANT

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
#endif

void static
CALC (double *r, double *s, int interv, int signctrl)
{
  int i;
  for (i = 0; i < SIZE; i++)
    {
      r[i] = get_norm_mant (s[i], signctrl, interv);
    }
}

void
TEST (void)
{
  int i, sign;
  UNION_TYPE (AVX512F_LEN, d) res1, res2, res3, src;
  MASK_TYPE mask = MASK_VALUE;
  double res_ref[SIZE];
  int interv = _MM_MANT_NORM_p5_1;
  int signctrl = _MM_MANT_SIGN_src;

  sign = -1;
  for (i = 0; i < SIZE; i++)
    {
      src.a[i] = 34.67 * i * sign;
      sign = sign * -1;
    }
  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_getmant_pd) (src.x, interv, signctrl);
  res2.x =
    INTRINSIC (_mask_getmant_pd) (res2.x, mask, src.x, interv,
				  signctrl);
  res3.x =
    INTRINSIC (_maskz_getmant_pd) (mask, src.x, interv, signctrl);

  CALC (res_ref, src.a, interv, signctrl);

  if (UNION_CHECK (AVX512F_LEN, d) (res1, res_ref))
    abort ();

  MASK_MERGE (d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res2, res_ref))
    abort ();

  MASK_ZERO (d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res3, res_ref))
    abort ();
}
