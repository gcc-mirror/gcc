/* Verify that overloaded built-ins for vec_unpackh and vec_unpackl with pixel
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2" } */

#include <altivec.h>

vector unsigned int
testf_el (vector pixel vpx2)
{
  return vec_unpackl (vpx2);
}

vector unsigned int
testf_eh (vector pixel vpx2)
{
  return vec_unpackh (vpx2);
}

/* { dg-final { scan-assembler-times "vupkhpx" 1 } } */
/* { dg-final { scan-assembler-times "vupklpx" 1 } } */
