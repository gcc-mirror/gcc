/* Verify that overloaded built-ins for vec_st with float
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2" } */

#include <altivec.h>

void
testst_1 (vector float vf1, int i1, vector float * vfp)
{
	return vec_st(vf1, i1, vfp);
}

void
testst_2 (vector float vf1, int i1, float * fp)
{
	return vec_st(vf1, i1, fp);
}

void
testst_cst1 (vector float vf1, int i1, vector float * vfp)
{
	return vec_st(vf1, 16, vfp);
}

void
testst_cst2 (vector float vf1, int i1, float * fp)
{
	return vec_st(vf1, 24, fp);
}

/* { dg-final { scan-assembler-times {\m(?:stvx|stxv|stxvx)\M} 4 } } */
