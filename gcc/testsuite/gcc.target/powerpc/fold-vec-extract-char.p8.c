/* Verify that overloaded built-ins for vec_extract() with char
   inputs produce the right code with a P8 (LE or BE) target.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O2" } */

// six tests total. Targeting P8LE / P8BE.
// P8 LE variable offset: rldicl, subfic, sldi, mtvsrd, xxpermdi, vslo, mfvsrd, sradi, rlwinm, (extsb)
// P8 LE constant offset: vspltb, mfvsrd, rlwinm, (extsb)
// P8 BE variable offset:                 sldi, mtvsrd, xxpermdi, vslo, mfvsrd, sradi, rlwinm, (extsb)
// P8 BE constant offset: vspltb, mfvsrd, rlwinm, (extsb)

/* { dg-final { scan-assembler-times {\mrldicl\M} 3 { target { le } } } } */
/* { dg-final { scan-assembler-times {\msubfic\M} 3 { target { le } } } } */
/* { dg-final { scan-assembler-times {\msldi\M} 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mmtvsrd\M} 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mxxpermdi\M} 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mvslo\M} 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mmfvsrd\M} 6 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\msrdi\M} 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times "extsb" 2 } } */
/* { dg-final { scan-assembler-times {\mvspltb\M} 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mrlwinm\M} 4 { target lp64 } } } */

/* multiple codegen variations for -m32. */
/* { dg-final { scan-assembler-times {\mrlwinm\M} 3 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mstxvw4x\M} 6 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mlbz\M} 6 { target ilp32 } } } */


#include <altivec.h>

unsigned char
testbc_var (vector bool char vbc2,signed int si)
{
  return vec_extract (vbc2, si);
}

signed char
testsc_var (vector signed char vsc2, signed int si)
{
  return vec_extract (vsc2, si);
}

unsigned char
testuc_var (vector unsigned char vuc2, signed int si)
{
  return vec_extract (vuc2, si);
}

unsigned char
testbc_cst (vector bool char vbc2)
{
  return vec_extract (vbc2, 12);
}

signed char
testsc_cst (vector signed char vsc2)
{
  return vec_extract (vsc2, 12);
}

unsigned char
testuc_cst (vector unsigned char vuc2)
{
  return vec_extract (vuc2, 12);
}

