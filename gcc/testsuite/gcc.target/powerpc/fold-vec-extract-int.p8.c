/* Verify that overloaded built-ins for vec_extract() with int
   inputs produce the right code with a P8 (LE or BE) target.  */

/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

// Targeting P8 (LE) and (BE).  6 tests total.
// P8 LE constant:  vspltw, mfvsrwz, (1:extsw/2:rldicl)
// P8 LE variables: subfic,  sldi, mtvsrd, xxpermdi, vslo, mfvsrd, sradi, (1:extsw/5:rldicl))
// P8 BE constant:  vspltw, mfvsrwz, (1:extsw/2:rldicl)
// P8 BE variables:                  sldi, mtvsrd, xxpermdi, vslo, mfvsrd, sradi, (1:extsw/2:rldicl))

/* { dg-final { scan-assembler-times {\mvspltw\M} 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mmfvsrwz\M} 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mrldicl\M} 5 { target { le } } } } */
/* { dg-final { scan-assembler-times {\mrldicl\M} 2 { target { lp64 && be } } } } */
/* { dg-final { scan-assembler-times {\msubfic\M} 3 { target { le } } } } */
/* { dg-final { scan-assembler-times {\msldi\M} 3  { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mmtvsrd\M} 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mxxpermdi\M} 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mvslo\M} 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mmfvsrd\M} 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\msrdi\M} 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mextsw\M} 2 { target lp64 } } } */


/* { dg-final { scan-assembler-times {\mli\M} 6 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mrlwinm\M} 3 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mstxvw4x\M} 6 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mlwz\M} 6 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\maddi?\M} 9 { target ilp32 } } } */



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

