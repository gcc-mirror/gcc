/* Verify that overloaded built-ins for vec_extract() with long long
   inputs produce the right code with a P8 (LE or BE) target.  */

/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O2" } */

// Targeting P8LE and P8BE, six tests total.
// P8 (LE) constants: mfvsrd
// P8 (LE) variables: addi,xxpermdi,mr,stxvd2x|stxvd4x,rldicl,sldi,ldx,blr
// P8 (BE) constants: mfvsrd
// P8 (BE) Variables: addi,xxpermdi,rldicl,mr,stxvd2x|stxvd4x,sldi,ldx,blr

/* { dg-final { scan-assembler-times {\maddi\M} 6 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\maddi\M} 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\madd\M} 3 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mlwz\M} 11 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mxxpermdi\M} 3 { target le } } } */
/* { dg-final { scan-assembler-times {\mxxpermdi\M} 2 { target { be && ilp32 } } } } */
/* { dg-final { scan-assembler-times {\mxxpermdi\M} 3 { target { be && lp64 } } } } */
/* { dg-final { scan-assembler-times {\mstxvd2x\M|\mstxvw4x\M} 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mstxvd2x\M|\mstxvw4x\M} 4 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mrldicl\M|\mrldic\M|\mrlwinm\M} 3 } } */
/* { dg-final { scan-assembler-times {\mmfvsrd\M} 3 { target { lp64 } } } } */
/* { dg-final { scan-assembler-times {\mmfvsrd\M} 0 { target { be && ilp32 } } } } */
/* { dg-final { scan-assembler-times {\mmtvsrd\M} 0 { target { lp64 } } } } */
/* { dg-final { scan-assembler-times {\mmtvsrd\M} 0 { target { be && ilp32 } } } } */

#include <altivec.h>

unsigned long long
testbl_var (vector bool long long vbl2, signed int si)
{
  return vec_extract (vbl2, si);
}

signed long long
testsl_var (vector signed long long vsl2, signed int si)
{
  return vec_extract (vsl2, si);
}

unsigned long long
testul_var (vector unsigned long long vul2, signed int si)
{
  return vec_extract (vul2, si);
}

unsigned long long
testbl_cst (vector bool long long vbl2)
{
  return vec_extract (vbl2, 1);
}

signed long long
testsl_cst (vector signed long long vsl2)
{
  return vec_extract (vsl2, 1);
}

unsigned long long
testul_cst (vector unsigned long vul2)
{
  return vec_extract (vul2, 1);
}

