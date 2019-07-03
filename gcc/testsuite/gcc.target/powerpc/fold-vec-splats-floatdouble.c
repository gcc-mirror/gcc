/* Verify that overloaded built-ins for vec_splat with float and
   double inputs for VSX produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O1" } */

#include <altivec.h>

vector float
test1f (float x)
{
  return vec_splats (x);
}

vector double
test1d (double x)
{
  return vec_splats (x);
}

// double test generates the permute instruction.
/* { dg-final { scan-assembler-times "xxpermdi" 1 } } */

// float test generates a convert (double to single non-signalling) followed by a splat.
/* { dg-final { scan-assembler-times {\mxscvdpspn?\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvspltw\M|\mxxspltw\M} 1 } } */
