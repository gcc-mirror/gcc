/* Verify that overloaded built-ins for vec_extract() with char
   inputs produce the right code with a P9 (LE) target.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2 " } */

/* { dg-final { scan-assembler-times {\mli\M} 3 { target lp64 } } } */
/*  Endian sensitive, vextubrx or vextublx.  */
/* { dg-final { scan-assembler-times "vextubrx|vextublx" 6 { target lp64 } } } */
/* { dg-final { scan-assembler-times "extsb" 2 } } */

/* { dg-final { scan-assembler-times "stxv" 6 { target ilp32 } } } */
/* { dg-final { scan-assembler-times "lbz" 6 { target ilp32 } } } */


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

