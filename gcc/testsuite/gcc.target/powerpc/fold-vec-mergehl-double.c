/* Verify that overloaded built-ins for vec_splat with float and
   double inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */

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

/* vec_merge with doubles tend to just use xxpermdi (3 ea for BE, 1 ea for LE).  */
/* { dg-final { scan-assembler-times "xxpermdi" 2  { target { powerpc*le-*-* } }    } } */
/* { dg-final { scan-assembler-times "xxpermdi" 6  { target { powerpc-*-* } }     } } */

