/* Verify that overloaded built-ins for vec_unpackh and vec_unpackl with int
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mpower8-vector -O2" } */

#include <altivec.h>

vector bool long long
testbi_l (vector bool int vbi2)
{
  return vec_unpackl (vbi2);
}

vector signed long long
testsi_l (vector signed int vsi2)
{
  return vec_unpackl (vsi2);
}

vector bool long long
testbi_h (vector bool int vbi2)
{
  return vec_unpackh (vbi2);
}

vector signed long long
testsi_h (vector signed int vsi2)
{
  return vec_unpackh (vsi2);
}

/* { dg-final { scan-assembler-times "vupkhsw" 2 } } */
/* { dg-final { scan-assembler-times "vupklsw" 2 } } */
