/* { dg-do compile { target lp64 } } */
/* { dg-options "-O -mlittle-endian -fno-stack-clash-protection -g" } */
/* { dg-final { check-function-bodies "**" "" } } */

#pragma GCC aarch64 "arm_sve.h"

/*
** callee1:
**	...
**	ldr	(z[0-9]+), \[x1, #3, mul vl\]
**	...
**	st4h	{z[0-9]+\.h - \1\.h}, p0, \[x0\]
**	st2h	{z3\.h - z4\.h}, p1, \[x0\]
**	st3h	{z5\.h - z7\.h}, p2, \[x0\]
**	ret
*/
void __attribute__((noipa))
callee1 (void *x0, svbfloat16x3_t z0, svbfloat16x2_t z3, svbfloat16x3_t z5,
	 svbfloat16x4_t stack1, svbfloat16_t stack2, svbool_t p0,
	 svbool_t p1, svbool_t p2)
{
  svst4_bf16 (p0, x0, stack1);
  svst2_bf16 (p1, x0, z3);
  svst3_bf16 (p2, x0, z5);
}

/*
** callee2:
**	ptrue	p3\.b, all
**	ld1h	(z[0-9]+\.h), p3/z, \[x2\]
**	st1h	\1, p0, \[x0\]
**	st2h	{z3\.h - z4\.h}, p1, \[x0\]
**	st3h	{z0\.h - z2\.h}, p2, \[x0\]
**	ret
*/
void __attribute__((noipa))
callee2 (void *x0, svbfloat16x3_t z0, svbfloat16x2_t z3, svbfloat16x3_t z5,
	 svbfloat16x4_t stack1, svbfloat16_t stack2, svbool_t p0,
	 svbool_t p1, svbool_t p2)
{
  svst1_bf16 (p0, x0, stack2);
  svst2_bf16 (p1, x0, z3);
  svst3_bf16 (p2, x0, z0);
}

void __attribute__((noipa))
caller (void *x0)
{
  svbool_t pg;
  pg = svptrue_b8 ();
  callee1 (x0,
	   svld3_vnum_bf16 (pg, x0, -9),
	   svld2_vnum_bf16 (pg, x0, -2),
	   svld3_vnum_bf16 (pg, x0, 0),
	   svld4_vnum_bf16 (pg, x0, 8),
	   svld1_vnum_bf16 (pg, x0, 5),
	   svptrue_pat_b8 (SV_VL1),
	   svptrue_pat_b16 (SV_VL2),
	   svptrue_pat_b32 (SV_VL3));
}

/* { dg-final { scan-assembler {\tld3h\t{z0\.h - z2\.h}, p[0-7]/z, \[x0, #-9, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tld2h\t{z3\.h - z4\.h}, p[0-7]/z, \[x0, #-2, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tld3h\t{z5\.h - z7\.h}, p[0-7]/z, \[x0\]\n} } } */
/* { dg-final { scan-assembler {\tld4h\t{(z[0-9]+)\.h - z[0-9]+\.h}.*\tstr\t\1, \[x1\]\n} } } */
/* { dg-final { scan-assembler {\tld4h\t{z[0-9]+\.h - (z[0-9]+)\.h}.*\tstr\t\1, \[x1, #3, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tld1h\t(z[0-9]+\.h), p[0-7]/z, \[x0, #5, mul vl\]\n.*\tst1h\t\1, p[0-7], \[x2\]\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp0\.b, vl1\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp1\.h, vl2\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp2\.s, vl3\n} } } */
