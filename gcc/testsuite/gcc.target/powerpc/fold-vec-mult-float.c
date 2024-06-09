/* Verify that overloaded built-ins for vec_mul with float
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-options "-mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector float
test1 (vector float x, vector float y)
{
  return vec_mul (x, y);
}

/* { dg-final { scan-assembler-times "\[ \t\]xvmulsp" 1 } } */

