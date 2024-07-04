/* Verify that overloaded built-ins for vec_perm with char
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -O2" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector bool char
testbc (vector bool char vbc2, vector bool char vbc3,
	vector unsigned char vuc)
{
  return vec_perm (vbc2, vbc3, vuc);
}

vector signed char
testsc (vector signed char vsc2, vector signed char vsc3,
	vector unsigned char vuc)
{
  return vec_perm (vsc2, vsc3, vuc);
}

vector unsigned char
testuc (vector unsigned char vuc2, vector unsigned char vuc3,
	vector unsigned char vuc)
{
  return vec_perm (vuc2, vuc3, vuc);
}

/* { dg-final { scan-assembler-times {\m(?:v|xx)permr?\M} 3 } } */
