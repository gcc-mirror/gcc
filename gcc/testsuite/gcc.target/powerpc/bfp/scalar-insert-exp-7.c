/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */

/* This test should succeed only on 64-bit configurations.  */
#include <altivec.h>

__ieee128
insert_exponent (unsigned __int128 *significand_p,
		 unsigned long long int *exponent_p)
{
  unsigned __int128 significand = *significand_p;
  unsigned long long int exponent = *exponent_p;

  return __builtin_vec_scalar_insert_exp (significand, exponent); /* { dg-error "'__builtin_vsx_scalar_insert_exp_q' requires" } */
}
