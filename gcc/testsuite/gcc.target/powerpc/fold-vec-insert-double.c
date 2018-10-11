/* Verify that overloaded built-ins for vec_insert with 
   double inputs produce the right codegen.  */

/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */

#include <altivec.h>

vector double
testd_var (double d, vector double vd, signed int si)
{
  return vec_insert (d, vd, si);
}

vector double
testd_cst (double d, vector double vd)
{
  return vec_insert (d, vd, 1);
}
/* The number of xxpermdi instructions varies between
 P7,P8,P9, ensure at least one hit. */
/* { dg-final { scan-assembler {\mxxpermdi\M} } } */

/* { dg-final { scan-assembler-times {\mrldic\M|\mrlwinm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mstxvd2x\M|\mstxv\M|\mstvx\M} 1 } } */
/* { dg-final { scan-assembler-times {\mstfdx\M|\mstfd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mlxvd2x\M|\mlxv\M|\mlvx\M} 1 } } */

