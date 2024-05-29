/* Verify that overloaded built-ins for vec_splat with float and
   double inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-options "-mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector double
testd_l (vector double vd2, vector double vd3)
{
  return vec_mergel (vd2, vd3);
}

vector double
testd_h (vector double vd2, vector double vd3)
{
  return vec_mergeh (vd2, vd3);
}

/* { dg-final { scan-assembler-times "xxpermdi" 2 } } */

