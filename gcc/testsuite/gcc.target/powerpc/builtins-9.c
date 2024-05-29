/* { dg-do compile } */
/* { dg-options "-maltivec -mdejagnu-cpu=power8 -mvsx -O3" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector float
test_pack_float (vector double x, vector double y)
{
  return vec_pack (x, y);
}

/* { dg-final { scan-assembler-times "vmrgew" 1 { target be } } } */
/* { dg-final { scan-assembler-times "vmrgow"  1 { target le } } } */

/* { dg-final { scan-assembler-times "xvcvdpsp"  2 } } */
/* { dg-final { scan-assembler-times "xxpermdi"  2 } } */

