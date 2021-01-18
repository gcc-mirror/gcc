/* Verify that overloaded built-ins for vec_extract() with int
   inputs produce the right code with a P7 (BE) target.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power7 -O2 " } */

// Targeting P7 (BE).  6 tests total.
// P7 constant:   li, addi, stxvw4x, rldic, addi, lwzx/lwax
// P7 variables:  li, addi, stxvw4x, lwa/lwz

/* { dg-final { scan-assembler-times {\mli\M} 6 } } */
/* { dg-final { scan-assembler-times {\maddi\M|\madd\M} 9 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\maddi\M|\madd\M} 12 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mstxvw4x\M|\mstvx\M|\mstxv\M} 6 } } */
/* { dg-final { scan-assembler-times {\mrldic\M|\mrlwinm\M} 3 } } */
/* { dg-final { scan-assembler-times {\mlwz\M|\mlwa\M|\mlwzx\M|\mlwax\M} 6 } } */

#include <altivec.h>

unsigned int
testbi_var (vector bool int vbi2, signed int si)
{
  return vec_extract (vbi2, si);
}

signed int
testsi_var (vector signed int vsi2, signed int si)
{
  return vec_extract (vsi2, si);
}

unsigned int
testui_var (vector unsigned int vui2, signed int si)
{
  return vec_extract (vui2, si);
}

unsigned int
testbi_cst (vector bool int vbi2)
{
  return vec_extract (vbi2, 12);
}

signed int
testsi_cst (vector signed int vsi2)
{
  return vec_extract (vsi2, 12);
}

unsigned int
testui_cst (vector unsigned int vui2)
{
  return vec_extract (vui2, 12);
}

