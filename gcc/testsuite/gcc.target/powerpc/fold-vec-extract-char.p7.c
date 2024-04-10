/* Verify that overloaded built-ins for vec_extract() with char
   inputs produce the right code with a power7 (BE) target.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power7 -O2" } */

// Six tests total. Targeting P7 (BE).
// P7 variable offset:  addi, li,       stxvw4x, rldicl, add, lbz, (extsb)
// P7 const offset:           li, addi, stxvw4x,              lbz, (extsb)
/* one extsb (extend sign-bit) instruction generated for each test against
   unsigned types */

/* { dg-final { scan-assembler-times {\maddi\M} 6 } } */
/* { dg-final { scan-assembler-times {\mli\M} 6 } } */
/* { dg-final { scan-assembler-times {\mstxvw4x\M|\mstvx\M|\mstxv\M} 6 } } */
/* -m32 target uses rlwinm in place of rldicl. */
/* { dg-final { scan-assembler-times {\mrldicl\M|\mrlwinm\M} 3 } } */
/* { dg-final { scan-assembler-times {\madd\M} 3 } } */
/* { dg-final { scan-assembler-times {\mlbz\M} 6 } } */
/* { dg-final { scan-assembler-times {\mextsb\M} 2 } } */

#include <altivec.h>

unsigned char
testbc_var (vector bool char vbc2, signed int si)
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

