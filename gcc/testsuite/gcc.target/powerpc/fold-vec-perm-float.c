/* Verify that overloaded built-ins for vec_perm with float
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -O2" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector float
testf (vector float vf2, vector float vf3, vector unsigned char vuc)
{
  return vec_perm (vf2, vf3, vuc);
}

/* { dg-final { scan-assembler-times {\m(?:v|xx)permr?\M} 1 } } */
