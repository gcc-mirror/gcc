/* Verify that overloaded built-ins for vec_perm with double
   inputs produce the right code.  */

/* { dg-do compile } */
// vector double needs -mvsx.
/* { dg-options "-mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector double
testd (vector double vd2, vector double vd3, vector unsigned char vuc)
{
  return vec_perm (vd2, vd3, vuc);
}

/* { dg-final { scan-assembler-times {\m(?:v|xx)permr?\M} 1 } } */
