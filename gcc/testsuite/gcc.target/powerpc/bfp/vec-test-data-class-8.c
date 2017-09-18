/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mcpu=power9" } */

#include <altivec.h>
#include <stdlib.h>

/* Flags to select tests:
    0x40    Test for NaN
    0x20    Test for +Infinity
    0x10    Test for -Infinity
    0x08    Test for +Zero
    0x04    Test for -Zero
    0x02    Test for +Denormal
    0x01    Test for -Denormal  */

__vector bool int
test_nan (__vector float *p)
{
  __vector float source = *p;

  return vec_test_data_class (source, 0x40);
}

__vector bool int
test_infinity (__vector float *p)
{
  __vector float source = *p;

  return vec_test_data_class (source, 0x30);
}

__vector bool int
test_zero (__vector float *p)
{
  __vector float source = *p;

  return vec_test_data_class (source, 0x0c);
}

__vector bool int
test_denormal (__vector float *p)
{
  __vector float source = *p;

  return vec_test_data_class (source, 0x03);
}

float
float_scalar_insert_exp (unsigned int significand, unsigned int exponent)
{
  float result;
  unsigned int *result_as_uip = (unsigned int *) &result;

  *result_as_uip = (significand & ~0x800000) | ((exponent & 0xff) << 23);
  return result;
}

int
main ()
{
  __vector float argument;
  __vector bool result;

  unsigned int signaling_significand = 0x00a00000;
  unsigned int quiet_significand = 0x00c00000;
  unsigned int one_significand = 0x00800000;
  unsigned int three_significand = 0x00c00000;
  unsigned int five_significand = 0x00a00000;
  unsigned int zero_significand = 0x00000000;
  unsigned int minus_zero_significand = 0x80000000;

  /* A NaN is represented with the maximum biased exponent value and a
   *  non-zero fraction value. The sign bit ignored.  If the
   *  high-order bit of the fraction field is 0, then the NaN
   *  is a Signaling NaN.  Otherwise, it is a Quiet NaN.  */
  argument[0] = float_scalar_insert_exp (signaling_significand, 255);
  argument[1] = float_scalar_insert_exp (quiet_significand, 255);
  argument[2] = 1.0f;
  argument[3] = -0.07f;
  result = test_nan (&argument);
  if (!result[0] || !result[1] || result[2] || result[3])
    abort ();

  /* Infinity is represented by a biased exponent value of:
   *   255 in single format
   *   2047 in double format
   * and a zero fraction value.  The difference between +infinity and
   * -infinity is the value of the sign bit.  */
  argument[2] = float_scalar_insert_exp (zero_significand, 255);
  argument[3] = float_scalar_insert_exp (minus_zero_significand, 255);
  result = test_infinity (&argument);
  if (result[0] || result[1] || !result[2] || !result[3])
    abort ();

  /* A Zero value has a biased exponent value of zero and a zero
   *   fraction value.  The sign may be either positive or negative.  */
  argument[1] = float_scalar_insert_exp (minus_zero_significand, 0);
  argument[2] = float_scalar_insert_exp (zero_significand, 0);
  result = test_zero (&argument);
  if (result[0] || !result[1] || !result[2] || result[3])
    abort ();

  /* A Denormal number has a biased exponent value of zero and a
   *   non-zero fraction value.  */
  argument[0] = float_scalar_insert_exp (five_significand, 0);
  argument[3] = float_scalar_insert_exp (three_significand, 0);
  result = test_denormal (&argument);
  if (!result[0] || result[1] || result[2] || !result[3])
    abort ();
}
