/* Verify that overloaded built-ins for vec_st* with long long
   inputs produce the right code.  */

/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */

#include <altivec.h>

void
testst_1 (vector signed long long vsll1, int i1, vector signed long long * vsllp)
{
	return vec_st(vsll1, i1, vsllp);
}
void
testst_3 (vector unsigned long long vull1, int i1, vector unsigned long long * vsllp)
{
	return vec_st(vull1, i1, vsllp);
}
void
testst_5 (vector bool long long vbll1, int i1, vector bool long long * vbllp)
{
	return vec_st(vbll1, i1, vbllp);
}
void
testst_cst1 (vector signed long long vsll1, int i1, vector signed long long * vsllp)
{
	return vec_st(vsll1, 12, vsllp);
}
void
testst_cst3 (vector unsigned long long vull1, int i1, vector unsigned long long * vsllp)
{
	return vec_st(vull1, 24, vsllp);
}
void
testst_cst5 (vector bool long long vbll1, int i1, vector bool long long * vbllp)
{
	return vec_st(vbll1, 36, vbllp);
}

/* { dg-final { scan-assembler-times {\mstvx\M}  6 } } */
