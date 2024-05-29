/* Verify that overloaded built-ins for vec_div with float and
   double inputs for VSX produce the right results. */

/* { dg-do compile } */
/* { dg-options "-mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector double
test2 (vector double x, vector double y)
{
  return vec_div (x, y);
}

/* { dg-final { scan-assembler-times {\mxvdivdp\M} 1 } } */
