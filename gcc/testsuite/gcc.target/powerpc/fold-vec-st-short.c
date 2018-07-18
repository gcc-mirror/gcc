/* Verify that overloaded built-ins for vec_st* with short
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2" } */

#include <altivec.h>
// vector signed short vec_ld (int, const vector signed short *);
// void vec_st (vector signed short, int, vector signed short *);

void
testst_1 (vector signed short vss1, int i1, vector signed short * vssp)
{
	return vec_st(vss1, i1, vssp);
}
void
testst_2 (vector signed short vss1, int i1, signed short * ssp)
{
	return vec_st(vss1, i1, ssp);
}
void
testst_3 (vector unsigned short vus1, int i1, vector unsigned short * vusp)
{
	return vec_st(vus1, i1, vusp);
}
void
testst_4 (vector unsigned short vus1, int i1, unsigned short * usp)
{
	return vec_st(vus1, i1, usp);
}
void
testst_5 (vector bool short vbs1, int i1, vector bool short * vbsp)
{
	return vec_st(vbs1, i1, vbsp);
}
void
testst_6 (vector bool short vbs1, int i1, unsigned short * vusp)
{
	return vec_st(vbs1, i1, vusp);
}
void
testst_7 (vector bool short vbs1, int i1, signed short * vssp)
{
	return vec_st(vbs1, i1, vssp);
}
void
testst_cst1 (vector signed short vss1, int i1, vector signed short * vssp)
{
	return vec_st(vss1, 12, vssp);
}
void
testst_cst2 (vector signed short vss1, int i1, signed short * ssp)
{
	return vec_st(vss1, 16, ssp);
}
void
testst_cst3 (vector unsigned short vus1, int i1, vector unsigned short * vusp)
{
	return vec_st(vus1, 20, vusp);
}
void
testst_cst4 (vector unsigned short vus1, int i1, unsigned short * usp)
{
	return vec_st(vus1, 24, usp);
}
void
testst_cst5 (vector bool short vbs1, int i1, vector bool short * vbsp)
{
	return vec_st(vbs1, 28, vbsp);
}
void
testst_cst6 (vector bool short vbs1, int i1, unsigned short * vusp)
{
	return vec_st(vbs1, 32, vusp);
}
void
testst_cst7 (vector bool short vbs1, int i1, signed short * vssp)
{
	return vec_st(vbs1, 36, vssp);
}

/* { dg-final { scan-assembler-times {\mstvx\M} 14} } */
