/* Verify that overloaded built-ins for vec_sub with float
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -mno-vsx" } */

#include <altivec.h>

vector float
test1 (vector float x, vector float y)
{
  return vec_sub (x, y);
}

/* { dg-final { scan-assembler-times "vsubfp" 1 } } */

