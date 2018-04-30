/* Verify that overloaded built-ins for vec_madd with
   double inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */

#include <altivec.h>

vector double
testd_l (vector double vd2, vector double vd3, vector double vd4)
{
  return vec_madd (vd2, vd3, vd4);
}

/* { dg-final { scan-assembler-times "xvmaddmdp|xvmaddadp" 1 } } */

