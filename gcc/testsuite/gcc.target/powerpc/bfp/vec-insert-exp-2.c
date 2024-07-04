/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

__vector float
make_floats (__vector unsigned int *significands_p, 
	     __vector unsigned int *exponents_p)
{
  __vector unsigned int significands = *significands_p;
  __vector unsigned int exponents = *exponents_p;

  return __builtin_vec_insert_exp (significands, exponents); /* { dg-error "'__builtin_vsx_insert_exp_sp' requires" } */
}
