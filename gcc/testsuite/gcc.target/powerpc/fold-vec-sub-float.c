/* Verify that overloaded built-ins for vec_sub with float
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -mno-vsx" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector float
test1 (vector float x, vector float y)
{
  return vec_sub (x, y);
}

/* { dg-final { scan-assembler-times "vsubfp" 1 } } */

