/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mcpu=power9" } */

#include <altivec.h>
#include <stdbool.h>
#include <stdlib.h>

bool
test_denormal (double *p)
{
  double source = *p;

  /*
    0x40    Test for NaN
    0x20    Test for +Infinity
    0x10    Test for -Infinity
    0x08    Test for +Zero
    0x04    Test for -Zero
    0x02    Test for +Denormal
    0x01    Test for -Denormal
  */
  return scalar_test_data_class (source, 3);
}

int
main ()
{
  /* A Denormal number has a biased exponent value of zero and a
   *   non-zero fraction value.  */
  double denormal_plus = scalar_insert_exp (0x0008000000000000ULL, 0x0ULL);
  double denormal_minus = scalar_insert_exp (0x8008000000000000ULL, 0x0ULL);
  double not_denormal = scalar_insert_exp (0x8000000000000000ULL, 1023ULL);

  if (!test_denormal (&denormal_plus))
    abort ();
  if (!test_denormal (&denormal_minus))
    abort ();
  if (test_denormal (&not_denormal))
    abort ();
  return 0;
}

