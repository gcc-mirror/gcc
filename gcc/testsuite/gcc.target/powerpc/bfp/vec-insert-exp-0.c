/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9" } */

#include <altivec.h>

__vector float
make_floats (__vector unsigned int *significands_p, 
	     __vector unsigned int *exponents_p)
{
  __vector unsigned int significands = *significands_p;
  __vector unsigned int exponents = *exponents_p;

  return vec_insert_exp (significands, exponents);
}

/* { dg-final { scan-assembler "xviexpsp" } } */
