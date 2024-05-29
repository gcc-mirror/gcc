/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-require-effective-target has_arch_ppc64 } */

/* This test should succeed only on 64-bit configurations.  */
#include <altivec.h>

double
insert_exponent (unsigned long long int *significand_p,
		 unsigned long long int *exponent_p)
{
  unsigned long long int significand = *significand_p;
  unsigned long long int exponent = *exponent_p;

  return __builtin_vec_scalar_insert_exp (significand, exponent); /* { dg-error "'__builtin_vsx_scalar_insert_exp' requires" } */
}
