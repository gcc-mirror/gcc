/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mdejagnu-cpu=power9" } */

/* This test should succeed on 32-bit and 64-bit configurations.  */
#include <altivec.h>
#include <stdlib.h>

int
compare_exponents_lt (double *exponent1_p, double *exponent2_p)
{
  double exponent1 = *exponent1_p;
  double exponent2 = *exponent2_p;

  return scalar_cmp_exp_lt (exponent1, exponent2);
}

int
main ()
{
  double x = (double) (0x1100LL << 50);
  double y = (double) (0x1101LL << 50);
  double z = (double) (0x1101LL << 37);

  if (compare_exponents_lt (&x, &y))
    abort ();
  if (!compare_exponents_lt (&z, &x))
    abort ();
  return 0;
}
