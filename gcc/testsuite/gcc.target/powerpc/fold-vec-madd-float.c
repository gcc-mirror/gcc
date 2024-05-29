/* Verify that overloaded built-ins for vec_madd with float
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-options "-mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector float
testf_l (vector float vf2, vector float vf3, vector float vf4)
{
  return vec_madd (vf2, vf3, vf4);
}

/* { dg-final { scan-assembler-times "xvmaddmsp|xvmaddasp" 1 } } */

