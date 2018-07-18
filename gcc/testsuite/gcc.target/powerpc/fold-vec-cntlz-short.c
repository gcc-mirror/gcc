/* Verify that overloaded built-ins for vec_cntlz with int
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-maltivec -mpower8-vector -O2" } */

#include <altivec.h>

vector signed short
testsi (vector signed short vss2)
{
  return vec_cntlz (vss2);
}

vector unsigned short
testui (vector unsigned short vus2)
{
  return vec_cntlz (vus2);
}

/* { dg-final { scan-assembler-times "vclzh" 2 } } */
