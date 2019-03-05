/* Verify that overloaded built-ins for vec_extract() with long long
   inputs produce the right code with a P8 (LE or BE) target.  */

/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power7 -O2" } */

// Targeting P7. six tests total.
// P7 (m64) with constants: xxpermdi, stfd, ld
// P7 (m64) with variables: li, addi, stxvd2x, rldic, addi, ldx

// P7 (m32) with constants: [xxpermdi or li/lwz,li/lwz],stxvw4x/stfd,lwz,lwz, addi
// P7 (m32) with variables: li, addi/rlwinm, stxvd2x, rldic, addi/add, ldx/lwz

/* results. */
/* { dg-final { scan-assembler-times {\mstfd\M} 3 { target lp64 } } } */
/* -m32 target with constant test has a stxvw4x in place of a stfd. */
/* { dg-final { scan-assembler-times {\mstfd\M} 2 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mld\M} 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mlwz\M} 11 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mli\M} 3 { target lp64 } } } */
/* -m32 target with constant test uses (+2)li where the -m64 has an ld */
/* { dg-final { scan-assembler-times {\mli\M} 5 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\maddi\M} 6 } } */
/* { dg-final { scan-assembler-times {\mstxvd2x\M|\mstvx\M} 3 } } */
/* { dg-final { scan-assembler-times {\mrldic\M|\mrlwinm\M} 3 } } */
/* { dg-final { scan-assembler-times {\mldx\M} 3 { target lp64 } } } */


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

