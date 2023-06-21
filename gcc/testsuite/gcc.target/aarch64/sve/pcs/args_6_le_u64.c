/* { dg-do compile { target lp64 } } */
/* { dg-options "-O -mlittle-endian -fno-stack-clash-protection -fno-cprop-registers -fdisable-rtl-combine -g" } */
/* { dg-final { check-function-bodies "**" "" } } */

#pragma GCC aarch64 "arm_sve.h"

/*
** callee1:
**	...
**	ldr	(z[0-9]+), \[x1, #3, mul vl\]
**	...
**	st4d	{z[0-9]+\.d - \1\.d}, p0, \[x0\]
**	st2d	{z3\.d - z4\.d}, p1, \[x0\]
**	st3d	{z5\.d - z7\.d}, p2, \[x0\]
**	ret
*/
void __attribute__((noipa))
callee1 (void *x0, svuint64x3_t z0, svuint64x2_t z3, svuint64x3_t z5,
	 svuint64x4_t stack1, svuint64_t stack2, svbool_t p0,
	 svbool_t p1, svbool_t p2)
{
  svst4_u64 (p0, x0, stack1);
  svst2_u64 (p1, x0, z3);
  svst3_u64 (p2, x0, z5);
}

/*
** callee2:
**	ptrue	p3\.b, all
**	ld1d	(z[0-9]+\.d), p3/z, \[x2\]
**	st1d	\1, p0, \[x0\]
**	st2d	{z3\.d - z4\.d}, p1, \[x0\]
**	st3d	{z0\.d - z2\.d}, p2, \[x0\]
**	ret
*/
void __attribute__((noipa))
callee2 (void *x0, svuint64x3_t z0, svuint64x2_t z3, svuint64x3_t z5,
	 svuint64x4_t stack1, svuint64_t stack2, svbool_t p0,
	 svbool_t p1, svbool_t p2)
{
  svst1_u64 (p0, x0, stack2);
  svst2_u64 (p1, x0, z3);
  svst3_u64 (p2, x0, z0);
}

void __attribute__((noipa))
caller (void *x0)
{
  svbool_t pg;
  pg = svptrue_b8 ();
  callee1 (x0,
	   svld3_vnum_u64 (pg, x0, -9),
	   svld2_vnum_u64 (pg, x0, -2),
	   svld3_vnum_u64 (pg, x0, 0),
	   svld4_vnum_u64 (pg, x0, 8),
	   svld1_vnum_u64 (pg, x0, 5),
	   svptrue_pat_b8 (SV_VL1),
	   svptrue_pat_b16 (SV_VL2),
	   svptrue_pat_b32 (SV_VL3));
}

/* { dg-final { scan-assembler {\tld3d\t{z0\.d - z2\.d}, p[0-7]/z, \[x0, #-9, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tld2d\t{z3\.d - z4\.d}, p[0-7]/z, \[x0, #-2, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tld3d\t{z5\.d - z7\.d}, p[0-7]/z, \[x0\]\n} } } */
/* { dg-final { scan-assembler {\tld4d\t{(z[0-9]+)\.d - z[0-9]+\.d}.*\tstr\t\1, \[x1\]\n} } } */
/* { dg-final { scan-assembler {\tld4d\t{z[0-9]+\.d - (z[0-9]+)\.d}.*\tstr\t\1, \[x1, #3, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tld1d\t(z[0-9]+\.d), p[0-7]/z, \[x0, #5, mul vl\]\n.*\tst1d\t\1, p[0-7], \[x2\]\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp0\.b, vl1\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp1\.h, vl2\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp2\.s, vl3\n} } } */
