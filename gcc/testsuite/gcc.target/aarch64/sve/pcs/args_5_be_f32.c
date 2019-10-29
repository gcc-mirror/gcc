/* { dg-do compile } */
/* { dg-options "-O -mbig-endian -fno-stack-clash-protection -g" } */
/* { dg-final { check-function-bodies "**" "" } } */

#pragma GCC aarch64 "arm_sve.h"

/*
** callee:
**	addvl	sp, sp, #-1
**	str	p4, \[sp\]
**	ptrue	p4\.b, all
** (
**	ld1w	(z[0-9]+\.s), p4/z, \[x1, #1, mul vl\]
**	ld1w	(z[0-9]+\.s), p4/z, \[x1\]
**	st2w	{\2 - \1}, p0, \[x0\]
** |
**	ld1w	(z[0-9]+\.s), p4/z, \[x1\]
**	ld1w	(z[0-9]+\.s), p4/z, \[x1, #1, mul vl\]
**	st2w	{\3 - \4}, p0, \[x0\]
** )
**	st4w	{z0\.s - z3\.s}, p1, \[x0\]
**	st3w	{z4\.s - z6\.s}, p2, \[x0\]
**	st1w	z7\.s, p3, \[x0\]
**	ldr	p4, \[sp\]
**	addvl	sp, sp, #1
**	ret
*/
void __attribute__((noipa))
callee (void *x0, svfloat32x4_t z0, svfloat32x3_t z4, svfloat32x2_t stack,
	svfloat32_t z7, svbool_t p0, svbool_t p1, svbool_t p2, svbool_t p3)
{
  svst2 (p0, x0, stack);
  svst4 (p1, x0, z0);
  svst3 (p2, x0, z4);
  svst1_f32 (p3, x0, z7);
}

void __attribute__((noipa))
caller (void *x0)
{
  svbool_t pg;
  pg = svptrue_b8 ();
  callee (x0,
	  svld4_vnum_f32 (pg, x0, -8),
	  svld3_vnum_f32 (pg, x0, -3),
	  svld2_vnum_f32 (pg, x0, 0),
	  svld1_vnum_f32 (pg, x0, 2),
	  svptrue_pat_b8 (SV_VL1),
	  svptrue_pat_b16 (SV_VL2),
	  svptrue_pat_b32 (SV_VL3),
	  svptrue_pat_b64 (SV_VL4));
}

/* { dg-final { scan-assembler {\tld4w\t{z0\.s - z3\.s}, p[0-7]/z, \[x0, #-8, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tld3w\t{z4\.s - z6\.s}, p[0-7]/z, \[x0, #-3, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tld1w\tz7\.s, p[0-7]/z, \[x0, #2, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tmov\tx1, sp\n} } } */
/* { dg-final { scan-assembler {\tld2w\t{(z[0-9]+\.s) - z[0-9]+\.s}.*\tst1w\t\1, p[0-7], \[x1\]\n} } } */
/* { dg-final { scan-assembler {\tld2w\t{z[0-9]+\.s - (z[0-9]+\.s)}.*\tst1w\t\1, p[0-7], \[x1, #1, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp0\.b, vl1\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp1\.h, vl2\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp2\.s, vl3\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp3\.d, vl4\n} } } */
