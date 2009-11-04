/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-O2 -maltivec" } */

/* We should only produce one vspltw as we already splatted the value.  */
/* { dg-final { scan-assembler-times "vspltw" 1 } } */

#include <altivec.h>

vector float f(vector float a)
{
  vector float b = vec_splat (a, 2);
  return vec_splat (b, 0);
}


