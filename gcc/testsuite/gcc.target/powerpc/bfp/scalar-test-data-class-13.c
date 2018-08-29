/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mcpu=power9" } */

#include <altivec.h>
#include <stdbool.h>
#include <stdlib.h>

bool
test_zero (float *p)
{
  float source = *p;

  /*
    0x40    Test for NaN
    0x20    Test for +Infinity
    0x10    Test for -Infinity
    0x08    Test for +Zero
    0x04    Test for -Zero
    0x02    Test for +Denormal
    0x01    Test for -Denormal
  */
  return scalar_test_data_class (source, 12);
}

int
main ()
{
  /* A Zero value has a biased exponent value of zero and a zero
   * fraction value.  The sign may be either positive or negative.  */
  unsigned int zero_plus_image = 0x0;
  unsigned int zero_minus_image = 0x80000000;
  unsigned int non_zero_image = 0x60000000;

  float *zero_plus_p = (float *) &zero_plus_image;
  float *zero_minus_p = (float *) &zero_minus_image;
  float *not_zero_p = (float *) &non_zero_image;

  if (!test_zero (zero_plus_p))
    abort ();
  if (!test_zero (zero_minus_p))
    abort ();
  if (test_zero (not_zero_p))
    abort ();
  return 0;
}
