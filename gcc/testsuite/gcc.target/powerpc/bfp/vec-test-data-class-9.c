/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mdejagnu-cpu=power9" } */

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

__vector bool long long int
test_nan (__vector double *p)
{
  __vector double source = *p;

  return vec_test_data_class (source, 0x40);
}

__vector bool long long int
test_infinity (__vector double *p)
{
  __vector double source = *p;

  return vec_test_data_class (source, 0x30);
}

__vector bool long long int
test_zero (__vector double *p)
{
  __vector double source = *p;

  return vec_test_data_class (source, 0x0c);
}

__vector bool long long int
test_denormal (__vector double *p)
{
  __vector double source = *p;

  return vec_test_data_class (source, 0x03);
}

int
main ()
{
  __vector double special_argument;
  __vector double nonspecial_argument;
  __vector bool long long int result;

  unsigned long long int signaling_significand =
    0x0017000000000000ULL;
  unsigned long long int quiet_significand =
    0x001f000000000000ULL;
  unsigned long long int one_significand =
    0x0010000000000000ULL;
  unsigned long long int three_significand =
    0x0018000000000000ULL;
  unsigned long long int five_significand =
    0x0014000000000000ULL;
  unsigned long long int zero_significand =
    0x0000000000000000ULL;
  unsigned long long int minus_zero_significand =
    0x8000000000000000ULL;

  nonspecial_argument[0] = -3.825;
  nonspecial_argument[1] = 3.14159;

  /* A NaN is represented with the maximum biased exponent value and a
   *  non-zero fraction value. The sign bit ignored.  If the
   *  high-order bit of the fraction field is 0, then the NaN
   *  is a Signaling NaN.  Otherwise, it is a Quiet NaN.  */
  special_argument[0] = scalar_insert_exp (signaling_significand, 2047);
  special_argument[1] = scalar_insert_exp (quiet_significand, 2047);
  result = test_nan (&special_argument);
  if (!result[0] || !result[1])
    abort ();
  result = test_nan (&nonspecial_argument);
  if (result[0] || result[1])
    abort ();

  /* Infinity is represented by a biased exponent value of:
   *   255 in single format
   *   2047 in double format
   * and a zero fraction value.  The difference between +infinity and
   * -infinity is the value of the sign bit.  */
  special_argument[0] = scalar_insert_exp (zero_significand, 2047);
  special_argument[1] = scalar_insert_exp (minus_zero_significand, 2047);
  result = test_infinity (&special_argument);
  if (!result[0] || !result[1])
    abort ();
  result = test_infinity (&nonspecial_argument);
  if (result[0] || result[1])
    abort ();

  /* A Zero value has a biased exponent value of zero and a zero
   *   fraction value.  The sign may be either positive or negative.  */
  special_argument[0] = scalar_insert_exp (minus_zero_significand, 0);
  special_argument[1] = scalar_insert_exp (zero_significand, 0);
  result = test_zero (&special_argument);
  if (!result[0] || !result[1])
    abort ();
  result = test_zero (&nonspecial_argument);
  if (result[0] || result[1])
    abort ();

  /* A Denormal number has a biased exponent value of zero and a
   *   non-zero fraction value.  */
  special_argument[0] = scalar_insert_exp (five_significand, 0);
  special_argument[1] = scalar_insert_exp (three_significand, 0);
  result = test_denormal (&special_argument);
  if (!result[0] || !result[1])
    abort ();
  result = test_denormal (&nonspecial_argument);
  if (result[0] || result[1])
    abort ();
  return 0;
}

