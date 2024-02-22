/* Verify that overloaded built-ins for vec_extract() with short
   inputs produce the right results with a P8 (LE or BE) target.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2" } */

// six tests total. Targeting P8, both LE and BE.
// p8 (le) variable offset: rldicl, subfic, sldi, mtvsrd, xxpermdi, vslo, mfvsrd, srdi, (1:extsh/2:rlwinm)
// p8 (le) const offset:                          mtvsrd,                                (1:extsh/2:rlwinm)
// p8 (be) var offset:                      sldi, mtvsrd, xxpermdi, vslo, mfvsrd, srdi, (1:extsh:2:rlwinm)
// p8 (be) const offset:    vsplth,               mfvsrd,                                (1:extsh/2:rlwinm)

// * - each of the above will have an extsh if the argument is signed.
// * - bool and unsigned tests also have an rlwinm.

/* { dg-final { scan-assembler-times "rldicl" 3 {target { lp64 && le } } } } */
/* { dg-final { scan-assembler-times "subfic" 3 {target { lp64 && le } } } } */
/* { dg-final { scan-assembler-times "vsplth" 3 { target { lp64 && be } } } } */
/* { dg-final { scan-assembler-times "sldi" 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times "mtvsrd" 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times "xxpermdi" 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times "vslo" 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times "mfvsrd" 6 { target lp64 } } } */
/* { dg-final { scan-assembler-times "srdi" 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times "extsh" 2 { target lp64 } } } */
/* { dg-final { scan-assembler-times "rlwinm" 4 { target lp64 } } } */

/* -m32 codegen tests. */
/* { dg-final { scan-assembler-times {\mli\M} 6 { target ilp32 } } } */
/* { dg-final { scan-assembler-times "stxvw4x" 6 { target ilp32 } } } */
/* add and rlwinm instructions only on the variable tests. */
/* { dg-final { scan-assembler-times {\madd\M} 3 { target ilp32 } } } */
/* { dg-final { scan-assembler-times "rlwinm" 3 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\maddi\M} 9 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mlha\M|\mlhz\M} 6 { target ilp32 } } } */


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

