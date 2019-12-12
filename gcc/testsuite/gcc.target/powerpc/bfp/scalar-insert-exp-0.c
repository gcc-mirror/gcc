/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9" } */

/* This test should succeed only on 64-bit configurations.  */
#include <altivec.h>

double
insert_exponent (unsigned long long int *significand_p,
		 unsigned long long int *exponent_p)
{
  unsigned long long int significand = *significand_p;
  unsigned long long int exponent = *exponent_p;

  return scalar_insert_exp (significand, exponent);
}

/* { dg-final { scan-assembler "xsiexpdp" } } */
