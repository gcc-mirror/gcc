/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector float
foo_p (float *a)
{
  return (vector float) { *a, *a, *a, *a };
}

vector float
foo_p2 (float *a)
{
  return vec_splats (*a);
}

/* { dg-final { scan-assembler-times {\mlxsiwzx\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxxspltw\M} 2 } } */
/* { dg-final { scan-assembler-not {\mlxsspx\M} } } */
/* { dg-final { scan-assembler-not {\mxscvdpspn\M} } } */
