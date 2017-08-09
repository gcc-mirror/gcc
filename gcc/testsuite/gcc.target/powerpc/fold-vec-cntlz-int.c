/* Verify that overloaded built-ins for vec_cntlz with int
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-maltivec -mpower8-vector -O2" } */

#include <altivec.h>

vector signed int
testsi (vector signed int vsi2)
{
  return vec_cntlz (vsi2);
}

vector unsigned int
testui (vector unsigned int vui2)
{
  return vec_cntlz (vui2);
}

/* { dg-final { scan-assembler-times "vclzw" 2 } } */
