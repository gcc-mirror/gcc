/* Verify that overloaded built-ins for vec_abs with float and
   double inputs for VSX produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */

#include <altivec.h>

vector float
test1 (vector float x)
{
  return vec_abs (x);
}

vector double
test2 (vector double x)
{
  return vec_abs (x);
}

/* { dg-final { scan-assembler-times "xvabssp" 1 } } */
/* { dg-final { scan-assembler-times "xvabsdp" 1 } } */
