/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power8" } */

#include <altivec.h>

__vector double
make_doubles (__vector double *significands_p,
	      __vector unsigned long long int *exponents_p)
{
  __vector double significands = *significands_p;
  __vector unsigned long long int exponents = *exponents_p;

  return __builtin_vec_insert_exp (significands, exponents); /* { dg-error "builtin function '__builtin_vsx_insert_exp_dp' requires" } */
}
