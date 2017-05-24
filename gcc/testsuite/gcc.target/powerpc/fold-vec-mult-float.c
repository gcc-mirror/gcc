/* Verify that overloaded built-ins for vec_mul with float
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */

#include <altivec.h>

vector float
test1 (vector float x, vector float y)
{
  return vec_mul (x, y);
}

/* { dg-final { scan-assembler-times "\[ \t\]xvmulsp" 1 } } */

