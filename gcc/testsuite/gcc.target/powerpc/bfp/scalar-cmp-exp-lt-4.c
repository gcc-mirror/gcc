/* { dg-do run  { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mcpu=power9" } */

/* This test should succeed on 32-bit and 64-bit configurations.  */
#include <altivec.h>
#include <stdlib.h>

char
compare_exponents_lt (double *exponent1_p, double *exponent2_p)
{
  double exponent1 = *exponent1_p;
  double exponent2 = *exponent2_p;

  if (scalar_cmp_exp_lt (exponent1, exponent2))
    return 't';
  else
    return 'f';
}

int
main ()
{
  double x = (double) (0x1100LL << 50);
  double y = (double) (0x1101LL << 50);
  double z = (double) (0x1101LL << 37);

  if (compare_exponents_lt (&x, &y) == 't')
    abort ();
  if (compare_exponents_lt (&z, &x) == 'f')
    abort ();
  return 0;
}
