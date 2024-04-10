/* Verify that overloaded built-ins for vec_pack with
   double inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */

#include <altivec.h>

// vector float vec_pack (vector double, vector double);

vector float
test_pack (vector double vd2, vector double vd3)
{
  return vec_pack (vd2, vd3);
}

/* { dg-final { scan-assembler-times "vmrgew" 1 { target be } } } */
/* { dg-final { scan-assembler-times "vmrgow"  1 { target le } } } */
