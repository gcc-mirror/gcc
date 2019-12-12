/* Verify that overloaded built-ins for vec_extract() with
   double inputs produce the right code with a P8 (LE or BE) target.  */

/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O2" } */

// targeting P8, BE and LE. 2 tests.
// P8 (LE) constants: xxlor
// P8 (LE) variables: xori, rldic, mtvsrd, xxpermdi, vslo, xxlor
// P8 (BE) constants: xxpermdi
// P8 (BE) variables:       rldic, mtvsrd, xxpermdi, vslo, xxlor

/* { dg-final { scan-assembler-times {\mxxlor\M} 2 { target { le && lp64 } } } } */
/* { dg-final { scan-assembler-times {\mxxlor\M} 1 { target { be && lp64 } } } } */
/* { dg-final { scan-assembler-times {\mxori\M} 1 { target { le } } } } */
/* { dg-final { scan-assembler-times {\mrldic\M|\mrlwinm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mmtvsrd\M} 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mxxpermdi\M} 1 { target { le } } } } */
/* { dg-final { scan-assembler-times {\mxxpermdi\M} 2 { target { lp64 && be } } } } */
/* { dg-final { scan-assembler-times {\mvslo\M} 1 { target lp64 } } } */

/* { dg-final { scan-assembler-times {\mstxvd2x\M} 1 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mxxpermdi\M} 1 { target ilp32 } } } */

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

