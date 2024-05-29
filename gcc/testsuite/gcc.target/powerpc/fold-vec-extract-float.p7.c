/* Verify that overloaded built-ins for vec_extract() with float
   inputs produce the right code with a P7 (BE) target.  */

/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power7 -O2 " } */
/* { dg-require-effective-target powerpc_vsx } */

// targeting P7 (BE), 2 tests.
// P7 constants: xscvspdp
// P7 variables: li, addi, stxvd2x, rldic, addi, lfsx

/* { dg-final { scan-assembler-times {\mxscvspdp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mli\M} 1 } } */
/* -m32 as an add in place of an addi. */
/* { dg-final { scan-assembler-times {\maddi?\M} 2 } } */
/* { dg-final { scan-assembler-times {\mstxvd2x\M|\mstvx\M|\mstxv\M} 1 } } */
/* -m32 uses rlwinm in place of rldic */
/* { dg-final { scan-assembler-times {\mrldic\M|\mrlwinm\M} 1 } } */
/* -m32 has lfs in place of lfsx */
/* { dg-final { scan-assembler-times {\mlfsx?\M} 1 } } */

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

