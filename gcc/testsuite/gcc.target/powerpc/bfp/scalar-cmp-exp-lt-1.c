/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

/* This test should succeed on 32-bit and 64-bit configurations.  */
#include <altivec.h>

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

/* { dg-final { scan-assembler "xscmpexpdp" } } */
