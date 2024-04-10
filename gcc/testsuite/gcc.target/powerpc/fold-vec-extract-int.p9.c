/* Verify that overloaded built-ins for vec_extract() with int
   inputs produce the right code with a P9 (LE) target.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2 " } */

// Targeting P9 (LE).  6 tests total.
// P9 constant:   li, vextuwrx, (1:extsw)
// P9 variables:  slwi, vextuwrx, (1:extsw)

/* { dg-final { scan-assembler-times {\mli\M} 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mslwi\M} 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mvextuwrx\M|\mvextuwlx\M} 6 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mextsw\M} 2 { target lp64 } } } */

/* { dg-final { scan-assembler-times {\mrlwinm\M} 3 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\madd\M} 3 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mstxv\M} 6 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mlwz\M} 6 { target ilp32 } } } */


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

