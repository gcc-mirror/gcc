/* Verify that overloaded built-ins for vec_madd with short
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2" } */

#include <altivec.h>

vector signed short
test_mad_sss (vector signed short vss2, vector signed short vss3,
	      vector signed short vss4)
{
  return vec_madd (vss2, vss3, vss4);
}

vector signed short
test_mad_suu (vector signed short vss2, vector unsigned short vus3,
		vector unsigned short vus4)
{
  return vec_madd (vss2, vus3, vus4);
}

vector signed short
test_mad_uss (vector unsigned short vus2, vector signed short vss3,
	      vector signed short vss4)
{
  return vec_madd (vus2, vss3, vss4);
}

vector unsigned short
test_mad_uuu (vector unsigned short vus2, vector unsigned short vus3,
		vector unsigned short vus4)
{
  return vec_madd (vus2, vus3, vus4);
}

/* { dg-final { scan-assembler-times "vmladduhm" 4 } } */
