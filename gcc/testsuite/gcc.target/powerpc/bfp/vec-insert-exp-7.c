/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */

#include <altivec.h>

__vector double
make_doubles (__vector double *significands_p,
	      __vector unsigned long long int *exponents_p)
{
  __vector double significands = *significands_p;
  __vector unsigned long long int exponents = *exponents_p;

  return __builtin_vec_insert_exp (significands, exponents); /* { dg-error "'__builtin_vsx_insert_exp_dp' requires" } */
}
