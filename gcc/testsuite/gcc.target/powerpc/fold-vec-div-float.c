/* Verify that overloaded built-ins for vec_div with float
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx" } */

#include <altivec.h>

vector float
test1 (vector float x, vector float y)
{
  return vec_div (x, y);
}

/* { dg-final { scan-assembler-times {\mxvdivsp\M} 1 } } */
