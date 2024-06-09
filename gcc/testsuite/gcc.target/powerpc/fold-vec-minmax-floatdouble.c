/* Verify that overloaded built-ins for vec_max with float and
   double inputs for VSX produce the right results.  */

/* { dg-do compile } */
/* { dg-options "-mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector float
test1_min (vector float x, vector float y)
{
  return vec_min (x, y);
}

vector double
test2_min (vector double x, vector double y)
{
  return vec_min (x, y);
}

vector float
test1_max (vector float x, vector float y)
{
  return vec_max (x, y);
}

vector double
test2_max (vector double x, vector double y)
{
  return vec_max (x, y);
}

/* { dg-final { scan-assembler-times "vminsp" 1 } } */
/* { dg-final { scan-assembler-times "vmindp" 1 } } */
/* { dg-final { scan-assembler-times "vmaxsp" 1 } } */
/* { dg-final { scan-assembler-times "vmaxdp" 1 } } */
