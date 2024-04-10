/* Verify that overloaded built-ins for vec_extract() with float
   inputs produce the right code.  */

/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2 " } */

/* { dg-final { scan-assembler-times {\mxscvspdp\M} 2 } } */
/* { dg-final { scan-assembler-times {\mrldicl\M} 1 { target le } } } */
/* { dg-final { scan-assembler-times {\msubfic\M} 1 { target le } } } */
/* { dg-final { scan-assembler-times {\msldi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mmtvsrdd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvslo\M} 1 } } */

#include <altivec.h>

float
testf_var (vector float vf2, signed int si)
{
  return vec_extract (vf2, si);
}

float
testf_cst (vector float vf2)
{
  return vec_extract (vf2, 12);
}

