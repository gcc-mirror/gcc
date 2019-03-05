/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mdejagnu-cpu=power9" } */

#include <altivec.h>
#include <stdbool.h>
#include <stdlib.h>

bool
test_infinity (__ieee128 *p)
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
  return scalar_test_data_class (source, 0x30);
}

int
main ()
{
  /* Infinity is represented by a biased exponent value of:
   *   255 in single format
   *   2047 in double format
   *   32767 in ieee128 format
   * and a zero fraction value.  */
  __int128 plus_significand = (__int128) 0;
  __int128 minus_significand = ((__int128) 0x1) << 127;
  __int128 a_number_significand = (((__int128) 0x1) << 112);

  unsigned long long int infinite_exponent = 0x7fff;
  unsigned long long int a_number_exponent = 16383;

  __ieee128 plus_infinity =
    scalar_insert_exp (plus_significand, infinite_exponent);
  __ieee128 minus_infinity =
    scalar_insert_exp (minus_significand, infinite_exponent);
  __ieee128 a_number =
    scalar_insert_exp (a_number_significand, a_number_exponent);

  if (!test_infinity (&plus_infinity))
    abort ();
  if (!test_infinity (&minus_infinity))
    abort ();
  if (test_infinity (&a_number))
    abort ();
  return 0;
}
