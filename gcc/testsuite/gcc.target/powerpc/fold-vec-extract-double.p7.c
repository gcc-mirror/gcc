/* Verify that overloaded built-ins for vec_extract() with
   double inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power7 -O2" } */
/* { dg-additional-options "-mbig-endian" { target powerpc*-*-linux* } } */
/* { dg-require-effective-target powerpc_altivec } */

// targeting P7 (BE), 2 tests.
// P7 constants: xxpermdi
// P7 variables: li, addi, rldic, addi, stxvd2x, lfdx

/* { dg-final { scan-assembler-times {\mxxpermdi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mli\M} 1 } } */
/* -m32 target has an 'add' in place of one of the 'addi'. */
/* { dg-final { scan-assembler-times {\maddi?\M} 2 } } */
/* -m32 target has a rlwinm in place of a rldic .  */
/* { dg-final { scan-assembler-times {\mrldic\M|\mrlwinm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mstxvd2x\M} 1 } } */
/* { dg-final { scan-assembler-times {\mlfdx?\M} 1 } } */

#include <altivec.h>

double
testd_var (vector double vd2, signed int si)
{
  return vec_extract (vd2, si);
}

double
testd_cst (vector double vd2)
{
  return vec_extract (vd2, 1);
}

