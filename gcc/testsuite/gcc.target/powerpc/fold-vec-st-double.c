/* Verify that overloaded built-ins for vec_st with 
   double inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-options "-mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

void
testst_1 (vector double vd1, int i1, vector double * vdp)
{
	return vec_st(vd1, i1, vdp);
}

void
testst_cst1 (vector double vd1, int i1, vector double * vdp)
{
	return vec_st(vd1, 12, vdp);
}

/* { dg-final { scan-assembler-times {\mstvx\M}  2 } } */
