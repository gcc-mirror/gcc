/* Verify that overloaded built-ins for vec_st* with char
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2" } */

#include <altivec.h>

void
testst_1 (vector signed char vsc1, int i1, vector signed char * vscp)
{
	return vec_st(vsc1, i1, vscp);
}

void
testst_2 (vector signed char vsc1, int i1, signed char * scp)
{
	return vec_st(vsc1, i1, scp);
}

void
testst_3 (vector unsigned char vuc1, int i1, vector unsigned char * vscp)
{
	return vec_st(vuc1, i1, vscp);
}

void
testst_4 (vector unsigned char vuc1, int i1, unsigned char * scp)
{
	return vec_st(vuc1, i1, scp);
}

void
testst_5 (vector bool char vbc1, int i1, vector bool char * vbcp)
{
	return vec_st(vbc1, i1, vbcp);
}

void
testst_6 (vector bool char vbc1, int i1, unsigned char * vucp)
{
	return vec_st(vbc1, i1, vucp);
}

void
testst_7 (vector bool char vbc1, int i1, signed char * vscp)
{
	return vec_st(vbc1, i1, vscp);
}

void
testst_cst1 (vector signed char vsc1, int i1, vector signed char * vscp)
{
	return vec_st(vsc1, 12, vscp);
}

void
testst_cst2 (vector signed char vsc1, int i1, signed char * scp)
{
	return vec_st(vsc1, 16, scp);
}

void
testst_cst3 (vector unsigned char vuc1, int i1, vector unsigned char * vscp)
{
	return vec_st(vuc1, 20, vscp);
}

void
testst_cst4 (vector unsigned char vuc1, int i1, unsigned char * scp)
{
	return vec_st(vuc1, 24, scp);
}

void
testst_cst5 (vector bool char vbc1, int i1, vector bool char * vbcp)
{
	return vec_st(vbc1, 28, vbcp);
}

void
testst_cst6 (vector bool char vbc1, int i1, unsigned char * vucp)
{
	return vec_st(vbc1, 32, vucp);
}

void
testst_cst7 (vector bool char vbc1, int i1, signed char * vscp)
{
	return vec_st(vbc1, 36, vscp);
}

/* { dg-final { scan-assembler-times {\m(?:stvx|stxv|stxvx)\M} 14 } } */
