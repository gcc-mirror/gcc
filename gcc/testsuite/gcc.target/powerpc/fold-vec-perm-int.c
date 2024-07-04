/* Verify that overloaded built-ins for vec_perm with int
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -O2" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector bool int
testbi (vector bool int vbi2, vector bool int vbi3,
	vector unsigned char vuc)
{
  return vec_perm (vbi2, vbi3, vuc);
}

vector signed int
testsi (vector signed int vsi2, vector signed int vsi3,
	vector unsigned char vuc)
{
  return vec_perm (vsi2, vsi3, vuc);
}

vector unsigned int
testui (vector unsigned int vui2, vector unsigned int vui3,
	vector unsigned char vuc)
{
  return vec_perm (vui2, vui3, vuc);
}

/* { dg-final { scan-assembler-times {\m(?:v|xx)permr?\M} 3 } } */
