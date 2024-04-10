/* Verify that overloaded built-ins for vec_extract() with short
   inputs produce the right code for a P9 target.  */

/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */

// six tests total. Targeting P9.
// p9 (le) variable offset: slwi, vextuhlx, extsh
// p9 (le) const offset:  li, vextuhlx, extsh

/* { dg-final { scan-assembler-times {\mslwi\M} 3 } } */
/* { dg-final { scan-assembler-times {\mli\M} 3 } } */
/* { dg-final { scan-assembler-times "vextuhrx" 6 { target le } } } */
/* { dg-final { scan-assembler-times "vextuhlx" 6 { target be } } } */
/* { dg-final { scan-assembler-times {\mextsh\M} 2 } } */

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

