/* Verify that overloaded built-ins for vec_msum with int
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2" } */

#include <altivec.h>

vector signed int
test_msum_si (vector signed short vss2, vector signed short vss3,
	   vector signed int vsi2)
{
  return vec_msum (vss2, vss3, vsi2);
}

vector unsigned int
test_msum_ui (vector unsigned short vus2, vector unsigned short vus3,
	   vector unsigned int vui2)
{
  return vec_msum (vus2, vus3, vui2);
}

/* { dg-final { scan-assembler-times "vmsumshm" 1 } } */
/* { dg-final { scan-assembler-times "vmsumuhm" 1 } } */
