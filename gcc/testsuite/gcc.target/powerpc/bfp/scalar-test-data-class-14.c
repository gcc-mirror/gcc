/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mcpu=power9" } */

#include <altivec.h>
#include <stdbool.h>
#include <stdlib.h>

bool
test_nan (__ieee128 *p)
{
  __ieee128 source = *p;

  /*
    0x40    Test for NaN
    0x20    Test for +Infinity
    0x10    Test for -Infinity
    0x08    Test for +Zero
    0x04    Test for -Zero
    0x02    Test for +Denormal
    0x01    Test for -Denormal
  */
  return scalar_test_data_class (source, 0x40);
}

int
main ()
{
  /* NaN is represented with the maximum biased exponent value and a
   *  non-zero fraction value. The sign bit ignored.  If the
   *  high-order bit of the fraction field is 0, then the NaN is a
   *  Signaling NaN.  Otherwise, it is a Quiet NaN.  */
  __int128 signal_significand = (__int128) 0xffffffff;
  __int128 quiet_significand = (((__int128) 0x1) << 112) | 0xffffffff;
  __int128 a_number_significand = (((__int128) 0x1) << 112);
  unsigned long long int nan_exponent = 0x7fff;
  unsigned long long int a_number_exponent = 16383;

  __ieee128 signaling_nan =
    scalar_insert_exp (signal_significand, nan_exponent);
  __ieee128 quiet_nan =
    scalar_insert_exp (quiet_significand, nan_exponent);
  __ieee128 a_number =
    scalar_insert_exp (a_number_significand, a_number_exponent);

  if (!test_nan (&signaling_nan))
    abort ();
  if (!test_nan (&quiet_nan))
    abort ();
  if (test_nan (&a_number))
    abort ();
  return 0;
}
