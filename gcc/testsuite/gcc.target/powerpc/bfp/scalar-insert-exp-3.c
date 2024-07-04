/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-require-effective-target has_arch_ppc64 } */

/* This test should succeed only on 64-bit configurations.  */
#include <altivec.h>

double
insert_exponent (double *significand_p,
		 unsigned long long int *exponent_p)
{
  double significand = *significand_p;
  unsigned long long int exponent = *exponent_p;

  return scalar_insert_exp (significand, exponent);
}

/* { dg-final { scan-assembler "xsiexpdp" } } */
