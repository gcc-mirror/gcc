/* Verify that overloaded built-ins for vec_extract() with float
   inputs produce the right code with a P8 (LE or BE) target.  */

/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O2" } */

// targeting P8, BE and LE. 2 tests.
// P8 (LE) constants: xxsldwi, xscvspdp
// P8 (LE) variables: rldicl, subfic, sldi, mtvsrd, xxpermdi, vslo, xscvspdp
// P8 (BE) constants:          xscvspdp
// P8 (BE) variables:                 sldi, mtvsrd, xxpermdi, vslo, xscvspdp

/* { dg-final { scan-assembler-times {\mxxsldwi\M} 1 { target { le } } } } */
/* { dg-final { scan-assembler-times {\mxscvspdp\M} 2 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mrldicl\M} 1 { target { le } } } } */
/* { dg-final { scan-assembler-times {\msubfic\M} 1 { target { le } } } } */
/* { dg-final { scan-assembler-times {\msldi\M} 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mmtvsrd\M} 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mxxpermdi\M} 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mvslo\M} 1 { target lp64 } } } */

/* { dg-final { scan-assembler-times {\mxscvspdp\M} 1 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mli\M} 1 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mrlwinm\M} 1 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mstxvd2x\M} 1 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\madd\M} 1 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mlfs\M} 1 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\maddi\M} 1 { target ilp32 } } } */


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

