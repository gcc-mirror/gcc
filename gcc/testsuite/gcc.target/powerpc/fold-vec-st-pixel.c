/* Verify that overloaded built-ins for vec_st* with pixel
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */

#include <altivec.h>

void
testst_1 (vector pixel vp1, int i1, vector pixel * vpp)
{
	return vec_st(vp1, i1, vpp);
}

void
testst_cst1 (vector pixel vp1, int i1, vector pixel * vpp)
{
	return vec_st(vp1, 12, vpp);
}

/* { dg-final { scan-assembler-times {\mstvx\M}  2 } } */
