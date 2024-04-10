/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { has_arch_ppc64 } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */

/* This test only runs on 32-bit configurations, where a compiler error
   should be issued because this builtin is not available on 
   32-bit configurations.  */

#include <altivec.h>

double
insert_exponent (unsigned long long int *significand_p,
		 unsigned long long int *exponent_p)
{
  unsigned long long int significand = *significand_p;
  unsigned long long int exponent = *exponent_p;

  return scalar_insert_exp (significand, exponent); /* { dg-error "'__builtin_vsx_scalar_insert_exp' requires the" } */
}
