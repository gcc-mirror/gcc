/* Verify that overloaded built-ins for vec_splat with float
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2" } */

#include <altivec.h>

vector float
testf_l (vector float vf2, vector float vf3)
{
  return vec_mergel (vf2, vf3);
}

vector float
testf_h (vector float vf2, vector float vf3)
{
  return vec_mergeh (vf2, vf3);
}

/* mergeh with floats use xxmrgh{l,w} (1 ea) insns.  */

/* { dg-final { scan-assembler-times "xxmrghw|vmrghw" 1 } } */
/* { dg-final { scan-assembler-times "xxmrglw|vmrglw" 1 } } */

