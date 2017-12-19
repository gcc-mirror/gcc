/* Verify that overloaded built-ins for vec_ld with 
   double inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */

#include <altivec.h>

vector double
testld_ll_vd (long long ll1, vector double vd)
{
  return vec_ld (ll1, &vd);
}

vector double
testld_cst_vd (long long ll1, vector double vd)
{
  return vec_ld (16, &vd);
}

/* { dg-final { scan-assembler-times {\mlvx\M|\mlxvd2x\M|\mlxv\M} 2 } } */
