/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */

#include <altivec.h>

__vector float
make_floats (__vector float *significands_p, 
	     __vector unsigned int *exponents_p)
{
  __vector float significands = *significands_p;
  __vector unsigned int exponents = *exponents_p;

  return __builtin_vec_insert_exp (significands, exponents); /* { dg-error "'__builtin_vsx_insert_exp_sp' requires" } */
}
