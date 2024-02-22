/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */

/* This test should succeed on 32-bit and 64-bit configurations.  */
#include <altivec.h>

int
compare_exponents_lt (double *exponent1_p, double *exponent2_p)
{
  double exponent1 = *exponent1_p;
  double exponent2 = *exponent2_p;

  return scalar_cmp_exp_lt (exponent1, exponent2);
}

/* { dg-final { scan-assembler "xscmpexpdp" } } */
