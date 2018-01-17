/* Verify that overloaded built-ins for vec_splat with pixel
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */

#include <altivec.h>

vector pixel
testf_el (vector pixel px2, vector pixel px3)
{
  return vec_mergel (px2, px3);
}

vector pixel
testf_eh (vector pixel px2, vector pixel px3)
{
  return vec_mergeh (px2, px3);
}

/* { dg-final { scan-assembler-times "vmrghh" 1 } } */
/* { dg-final { scan-assembler-times "vmrglh" 1 } } */

