/* { dg-do compile { target lp64 } } */
/* { dg-options "-O -mbig-endian -fno-stack-clash-protection -fno-cprop-registers -fdisable-rtl-combine -g" } */
/* { dg-final { check-function-bodies "**" "" } } */

#pragma GCC aarch64 "arm_sve.h"

/*
** callee1:
**	ptrue	p3\.b, all
**	...
**	ld1w	(z[0-9]+\.s), p3/z, \[x1, #3, mul vl\]
**	...
**	st4w	{z[0-9]+\.s - \1}, p0, \[x0\]
**	st2w	{z3\.s - z4\.s}, p1, \[x0\]
**	st3w	{z5\.s - z7\.s}, p2, \[x0\]
**	ret
*/
void __attribute__((noipa))
callee1 (void *x0, svfloat32x3_t z0, svfloat32x2_t z3, svfloat32x3_t z5,
	 svfloat32x4_t stack1, svfloat32_t stack2, svbool_t p0,
	 svbool_t p1, svbool_t p2)
{
  svst4_f32 (p0, x0, stack1);
  svst2_f32 (p1, x0, z3);
  svst3_f32 (p2, x0, z5);
}

/*
** callee2:
**	ptrue	p3\.b, all
**	ld1w	(z[0-9]+\.s), p3/z, \[x2\]
**	st1w	\1, p0, \[x0\]
**	st2w	{z3\.s - z4\.s}, p1, \[x0\]
**	st3w	{z0\.s - z2\.s}, p2, \[x0\]
**	ret
*/
void __attribute__((noipa))
callee2 (void *x0, svfloat32x3_t z0, svfloat32x2_t z3, svfloat32x3_t z5,
	 svfloat32x4_t stack1, svfloat32_t stack2, svbool_t p0,
	 svbool_t p1, svbool_t p2)
{
  svst1_f32 (p0, x0, stack2);
  svst2_f32 (p1, x0, z3);
  svst3_f32 (p2, x0, z0);
}

void __attribute__((noipa))
caller (void *x0)
{
  svbool_t pg;
  pg = svptrue_b8 ();
  callee1 (x0,
	   svld3_vnum_f32 (pg, x0, -9),
	   svld2_vnum_f32 (pg, x0, -2),
	   svld3_vnum_f32 (pg, x0, 0),
	   svld4_vnum_f32 (pg, x0, 8),
	   svld1_vnum_f32 (pg, x0, 5),
	   svptrue_pat_b8 (SV_VL1),
	   svptrue_pat_b16 (SV_VL2),
	   svptrue_pat_b32 (SV_VL3));
}

/* { dg-final { scan-assembler {\tld3w\t{z0\.s - z2\.s}, p[0-7]/z, \[x0, #-9, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tld2w\t{z3\.s - z4\.s}, p[0-7]/z, \[x0, #-2, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tld3w\t{z5\.s - z7\.s}, p[0-7]/z, \[x0\]\n} } } */
/* { dg-final { scan-assembler {\tld4w\t{(z[0-9]+\.s) - z[0-9]+\.s}.*\tst1w\t\1, p[0-7], \[x1\]\n} } } */
/* { dg-final { scan-assembler {\tld4w\t{z[0-9]+\.s - (z[0-9]+\.s)}.*\tst1w\t\1, p[0-7], \[x1, #3, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tld1w\t(z[0-9]+\.s), p[0-7]/z, \[x0, #5, mul vl\]\n.*\tst1w\t\1, p[0-7], \[x2\]\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp0\.b, vl1\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp1\.h, vl2\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp2\.s, vl3\n} } } */
