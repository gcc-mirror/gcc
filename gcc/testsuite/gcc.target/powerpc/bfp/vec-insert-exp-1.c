/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */

#include <altivec.h>

__vector double
make_doubles (__vector unsigned long long int *significands_p,
	      __vector unsigned long long int *exponents_p)
{
  __vector unsigned long long int significands = *significands_p;
  __vector unsigned long long int exponents = *exponents_p;

  return vec_insert_exp (significands, exponents);
}

/* { dg-final { scan-assembler "xviexpdp" } } */
