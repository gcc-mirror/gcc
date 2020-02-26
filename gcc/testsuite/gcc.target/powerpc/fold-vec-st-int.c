/* Verify that overloaded built-ins for vec_st* with int
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2" } */

#include <altivec.h>
// void vec_st (vector signed int, int, vector signed int *);

void
testst_1 (vector signed int vsi1, int i1, vector signed int * vsip)
{
	return vec_st(vsi1, i1, vsip);
}
void
testst_2 (vector signed int vsi1, int i1, signed int * sip)
{
	return vec_st(vsi1, i1, sip);
}
void
testst_3 (vector unsigned int vui1, int i1, vector unsigned int * vsip)
{
	return vec_st(vui1, i1, vsip);
}
void
testst_4 (vector unsigned int vui1, int i1, unsigned int * sip)
{
	return vec_st(vui1, i1, sip);
}
void
testst_5 (vector bool int vbi1, int i1, vector bool int * vbip)
{
	return vec_st(vbi1, i1, vbip);
}
void
testst_6 (vector bool int vbi1, int i1, unsigned int * vuip)
{
	return vec_st(vbi1, i1, vuip);
}
void
testst_7 (vector bool int vbi1, int i1, signed int * vsip)
{
	return vec_st(vbi1, i1, vsip);
}

void
testst_cst1 (vector signed int vsi1, int i1, vector signed int * vsip)
{
	return vec_st(vsi1, 12, vsip);
}
void
testst_cst2 (vector signed int vsi1, int i1, signed int * sip)
{
	return vec_st(vsi1, 16, sip);
}
void
testst_cst3 (vector unsigned int vui1, int i1, vector unsigned int * vsip)
{
	return vec_st(vui1, 20, vsip);
}
void
testst_cst4 (vector unsigned int vui1, int i1, unsigned int * sip)
{
	return vec_st(vui1, 24, sip);
}
void
testst_cst5 (vector bool int vbi1, int i1, vector bool int * vbip)
{
	return vec_st(vbi1, 28, vbip);
}
void
testst_cst6 (vector bool int vbi1, int i1, unsigned int * vuip)
{
	return vec_st(vbi1, 32, vuip);
}
void
testst_cst7 (vector bool int vbi1, int i1, signed int * vsip)
{
	return vec_st(vbi1, 36, vsip);
}

/* { dg-final { scan-assembler-times {\m(?:stvx|stxv|stxvx)\M} 14 } } */

