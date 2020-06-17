/* { dg-do compile { target lp64 } } */
/* { dg-options "-O -mbig-endian -fno-stack-clash-protection -g" } */
/* { dg-final { check-function-bodies "**" "" } } */

#pragma GCC aarch64 "arm_sve.h"

/*
** callee:
**	addvl	sp, sp, #-1
**	str	p4, \[sp\]
**	ptrue	p4\.b, all
** (
**	ld1h	(z[0-9]+\.h), p4/z, \[x1, #1, mul vl\]
**	ld1h	(z[0-9]+\.h), p4/z, \[x1\]
**	st2h	{\2 - \1}, p0, \[x0\]
** |
**	ld1h	(z[0-9]+\.h), p4/z, \[x1\]
**	ld1h	(z[0-9]+\.h), p4/z, \[x1, #1, mul vl\]
**	st2h	{\3 - \4}, p0, \[x0\]
** )
**	st4h	{z0\.h - z3\.h}, p1, \[x0\]
**	st3h	{z4\.h - z6\.h}, p2, \[x0\]
**	st1h	z7\.h, p3, \[x0\]
**	ldr	p4, \[sp\]
**	addvl	sp, sp, #1
**	ret
*/
void __attribute__((noipa))
callee (void *x0, svint16x4_t z0, svint16x3_t z4, svint16x2_t stack,
	svint16_t z7, svbool_t p0, svbool_t p1, svbool_t p2, svbool_t p3)
{
  svst2 (p0, x0, stack);
  svst4 (p1, x0, z0);
  svst3 (p2, x0, z4);
  svst1_s16 (p3, x0, z7);
}

void __attribute__((noipa))
caller (void *x0)
{
  svbool_t pg;
  pg = svptrue_b8 ();
  callee (x0,
	  svld4_vnum_s16 (pg, x0, -8),
	  svld3_vnum_s16 (pg, x0, -3),
	  svld2_vnum_s16 (pg, x0, 0),
	  svld1_vnum_s16 (pg, x0, 2),
	  svptrue_pat_b8 (SV_VL1),
	  svptrue_pat_b16 (SV_VL2),
	  svptrue_pat_b32 (SV_VL3),
	  svptrue_pat_b64 (SV_VL4));
}

/* { dg-final { scan-assembler {\tld4h\t{z0\.h - z3\.h}, p[0-7]/z, \[x0, #-8, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tld3h\t{z4\.h - z6\.h}, p[0-7]/z, \[x0, #-3, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tld1h\tz7\.h, p[0-7]/z, \[x0, #2, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tmov\tx1, sp\n} } } */
/* { dg-final { scan-assembler {\tld2h\t{(z[0-9]+\.h) - z[0-9]+\.h}.*\tst1h\t\1, p[0-7], \[x1\]\n} } } */
/* { dg-final { scan-assembler {\tld2h\t{z[0-9]+\.h - (z[0-9]+\.h)}.*\tst1h\t\1, p[0-7], \[x1, #1, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp0\.b, vl1\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp1\.h, vl2\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp2\.s, vl3\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp3\.d, vl4\n} } } */
