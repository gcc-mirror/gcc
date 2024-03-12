/* { dg-do compile { target lp64 } } */
/* { dg-options "-O -mbig-endian -fno-stack-clash-protection -g" } */
/* { dg-final { check-function-bodies "**" "" } } */

#pragma GCC aarch64 "arm_sve.h"

/*
** callee:
**	addvl	sp, sp, #-1
**	str	(p[4-7]), \[sp\]
**	ptrue	\1\.b, all
** (
**	ld1b	(z[0-9]+\.b), \1/z, \[x1, #1, mul vl\]
**	ld1b	(z[0-9]+\.b), \1/z, \[x1\]
**	st2b	{\3 - \2}, p0, \[x0\]
** |
**	ld1b	(z[0-9]+\.b), \1/z, \[x1\]
**	ld1b	(z[0-9]+\.b), \1/z, \[x1, #1, mul vl\]
**	st2b	{\4 - \5}, p0, \[x0\]
** )
**	st4b	{z0\.b - z3\.b}, p1, \[x0\]
**	st3b	{z4\.b - z6\.b}, p2, \[x0\]
**	st1b	z7\.b, p3, \[x0\]
**	ldr	\1, \[sp\]
**	addvl	sp, sp, #1
**	ret
*/
void __attribute__((noipa))
callee (void *x0, svint8x4_t z0, svint8x3_t z4, svint8x2_t stack,
	svint8_t z7, svbool_t p0, svbool_t p1, svbool_t p2, svbool_t p3)
{
  svst2 (p0, x0, stack);
  svst4 (p1, x0, z0);
  svst3 (p2, x0, z4);
  svst1_s8 (p3, x0, z7);
}

void __attribute__((noipa))
caller (void *x0)
{
  svbool_t pg;
  pg = svptrue_b8 ();
  callee (x0,
	  svld4_vnum_s8 (pg, x0, -8),
	  svld3_vnum_s8 (pg, x0, -3),
	  svld2_vnum_s8 (pg, x0, 0),
	  svld1_vnum_s8 (pg, x0, 2),
	  svptrue_pat_b8 (SV_VL1),
	  svptrue_pat_b16 (SV_VL2),
	  svptrue_pat_b32 (SV_VL3),
	  svptrue_pat_b64 (SV_VL4));
}

/* { dg-final { scan-assembler {\tld4b\t{z0\.b - z3\.b}, p[0-7]/z, \[x0, #-8, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tld3b\t{z4\.b - z6\.b}, p[0-7]/z, \[x0, #-3, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tld1b\tz7\.b, p[0-7]/z, \[x0, #2, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tmov\tx1, sp\n} } } */
/* { dg-final { scan-assembler {\tld2b\t{(z[0-9]+\.b) - z[0-9]+\.b}.*\tst1b\t\1, p[0-7], \[(?:x1|sp)\]\n} } } */
/* { dg-final { scan-assembler {\tld2b\t{z[0-9]+\.b - (z[0-9]+\.b)}.*\tst1b\t\1, p[0-7], \[(?:x1|sp), #1, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp0\.b, vl1\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp1\.h, vl2\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp2\.s, vl3\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp3\.d, vl4\n} } } */
