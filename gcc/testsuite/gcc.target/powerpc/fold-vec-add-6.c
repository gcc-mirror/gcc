/* Verify that overloaded built-ins for vec_add with float and
   double inputs for VSX produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-maltivec -mvsx" } */

#include <altivec.h>

vector float
test1 (vector float x, vector float y)
{
  return vec_add (x, y);
}

vector double
test2 (vector double x, vector double y)
{
  return vec_add (x, y);
}

/* { dg-final { scan-assembler-times "xvaddsp" 1 } } */
/* { dg-final { scan-assembler-times "xvadddp" 1 } } */
