/* { dg-do compile { target lp64 } } */
/* { dg-options "-O -mbig-endian -fno-stack-clash-protection -fno-cprop-registers -fdisable-rtl-combine -g" } */
/* { dg-final { check-function-bodies "**" "" } } */

#pragma GCC aarch64 "arm_sve.h"

/*
** callee1:
**	ptrue	p3\.b, all
**	...
**	ld1b	(z[0-9]+\.b), p3/z, \[x1, #3, mul vl\]
**	...
**	st4b	{z[0-9]+\.b - \1}, p0, \[x0\]
**	st2b	{z3\.b - z4\.b}, p1, \[x0\]
**	st3b	{z5\.b - z7\.b}, p2, \[x0\]
**	ret
*/
void __attribute__((noipa))
callee1 (void *x0, svmfloat8x3_t z0, svmfloat8x2_t z3, svmfloat8x3_t z5,
	 svmfloat8x4_t stack1, svmfloat8_t stack2, svbool_t p0,
	 svbool_t p1, svbool_t p2)
{
  svst4_mf8 (p0, x0, stack1);
  svst2_mf8 (p1, x0, z3);
  svst3_mf8 (p2, x0, z5);
}

/*
** callee2:
**	ptrue	p3\.b, all
**	ld1b	(z[0-9]+\.b), p3/z, \[x2\]
**	st1b	\1, p0, \[x0\]
**	st2b	{z3\.b - z4\.b}, p1, \[x0\]
**	st3b	{z0\.b - z2\.b}, p2, \[x0\]
**	ret
*/
void __attribute__((noipa))
callee2 (void *x0, svmfloat8x3_t z0, svmfloat8x2_t z3, svmfloat8x3_t z5,
	 svmfloat8x4_t stack1, svmfloat8_t stack2, svbool_t p0,
	 svbool_t p1, svbool_t p2)
{
  svst1_mf8 (p0, x0, stack2);
  svst2_mf8 (p1, x0, z3);
  svst3_mf8 (p2, x0, z0);
}

void __attribute__((noipa))
caller (void *x0)
{
  svbool_t pg;
  pg = svptrue_b8 ();
  callee1 (x0,
	   svld3_vnum_mf8 (pg, x0, -9),
	   svld2_vnum_mf8 (pg, x0, -2),
	   svld3_vnum_mf8 (pg, x0, 0),
	   svld4_vnum_mf8 (pg, x0, 8),
	   svld1_vnum_mf8 (pg, x0, 5),
	   svptrue_pat_b8 (SV_VL1),
	   svptrue_pat_b16 (SV_VL2),
	   svptrue_pat_b32 (SV_VL3));
}

/* { dg-final { scan-assembler {\tld3b\t{z0\.b - z2\.b}, p[0-7]/z, \[x0, #-9, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tld2b\t{z3\.b - z4\.b}, p[0-7]/z, \[x0, #-2, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tld3b\t{z5\.b - z7\.b}, p[0-7]/z, \[x0\]\n} } } */
/* { dg-final { scan-assembler {\tld4b\t{(z[0-9]+\.b) - z[0-9]+\.b}.*\tst1b\t\1, p[0-7], \[x1\]\n} } } */
/* { dg-final { scan-assembler {\tld4b\t{z[0-9]+\.b - (z[0-9]+\.b)}.*\tst1b\t\1, p[0-7], \[x1, #3, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tld1b\t(z[0-9]+\.b), p[0-7]/z, \[x0, #5, mul vl\]\n.*\tst1b\t\1, p[0-7], \[x2\]\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp0\.b, vl1\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp1\.h, vl2\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp2\.s, vl3\n} } } */
