/* Verify that overloaded built-ins for vec_extract() with short
   inputs produce the right code for a P7 (BE) target.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power7 -O2" } */

// six tests total. Targeting P7 BE.
// p7 (be) vars:                 li, addi,              stxvw4x, rldic, addi, lhax/lhzx
// P7 (be) constants:            li, addi,              stxvw4x, lha/lhz

/* { dg-final { scan-assembler-times {\mli\M} 6 } } */
/* { dg-final { scan-assembler-times {\maddi\M|\madd\M} 9 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\maddi\M|\madd\M} 12 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mrldic\M|\mrlwinm\M} 3 } } */
/* { dg-final { scan-assembler-times {\mstxvw4x\M|\mstvx\M} 6 } } */
/* { dg-final { scan-assembler-times "lhz|lha|lhzx|lhax" 6 } } */

#include <altivec.h>

unsigned short
testbi_cst (vector bool short vbs2)
{
  return vec_extract (vbs2, 12);
}

signed short
testsi_cst (vector signed short vss2)
{
  return vec_extract (vss2, 12);
}

unsigned short
testui_cst12 (vector unsigned short vus2)
{
  return vec_extract (vus2, 12);
}

unsigned short
testbi_var (vector bool short vbs2, signed int si)
{
  return vec_extract (vbs2, si);
}

signed short
testsi_var (vector signed short vss2, signed int si)
{
return vec_extract (vss2, si);
}

unsigned short
testui_var (vector unsigned short vus2, signed int si)
{
return vec_extract (vus2, si);
}

