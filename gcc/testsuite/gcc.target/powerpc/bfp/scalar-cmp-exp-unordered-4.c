/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mdejagnu-cpu=power9" } */

/* This test should succeed on 32-bit and 64-bit configurations.  */
#include <altivec.h>
#include <stdlib.h>

char
compare_exponents_unordered (double *exponent1_p, double *exponent2_p)
{
  double exponent1 = *exponent1_p;
  double exponent2 = *exponent2_p;

  /* This test succeeds if either exponent1 or exponent2 is NaN. */
  if (scalar_cmp_exp_unordered (exponent1, exponent2))
    return 't';
  else
    return 'f';
}

int
main ()
{
  /* NaN is denoted by exponent = 2047 and fraction != 0 */
  unsigned long long int nan_image = 0x7ff0000000000003LL;
  double *nan_ptr = (double *) &nan_image;

  double x = (double) (0x1100LL << 50);
  double y = (double) (0x1101LL << 50);
  double z = (double) (0x1101LL << 37);

  if (compare_exponents_unordered (&x, nan_ptr) == 'f')
    abort ();
  if (compare_exponents_unordered (&x, &z) == 't')
    abort ();
  return 0;
}
