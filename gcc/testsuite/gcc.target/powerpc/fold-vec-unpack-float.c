/* Verify that overloaded built-ins for vec_unpackh and vec_unpackl with float
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mpower8-vector -O2" } */

#include <altivec.h>

vector double
testf_l (vector float vf2)
{
  return vec_unpackl (vf2);
}

vector double
testf_h (vector float vf2)
{
  return vec_unpackh (vf2);
}

/* { dg-final { scan-assembler-times "xxsldwi" 4 } } */
/* { dg-final { scan-assembler-times "xvcvspdp" 2 } } */
