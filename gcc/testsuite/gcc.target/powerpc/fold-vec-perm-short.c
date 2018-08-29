/* Verify that overloaded built-ins for vec_perm with short
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2" } */

#include <altivec.h>

vector bool short
testbs (vector bool short vbs2, vector bool short vbs3,
	vector unsigned char vuc)
{
  return vec_perm (vbs2, vbs3, vuc);
}

vector signed short
testss (vector signed short vss2, vector signed short vss3, vector unsigned char vuc)
{
  return vec_perm (vss2, vss3, vuc);
}

vector unsigned short
testus (vector unsigned short vus2, vector unsigned short vus3, vector unsigned char vuc)
{
  return vec_perm (vus2, vus3, vuc);
}

/* { dg-final { scan-assembler-times "vperm" 3 } } */
