/* { dg-do compile } */
/* { dg-options "-O -fno-stack-clash-protection -g" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#include <arm_sve.h>

/*
** callee_float:
**	ptrue	p3\.b, all
**	ld1h	(z(?:2[4-9]|3[0-1]).h), p3/z, \[x4\]
**	st1h	\1, p2, \[x0\]
**	st1h	z4\.h, p1, \[x0\]
**	st1h	z5\.h, p1, \[x1\]
**	st1w	z6\.s, p1, \[x2\]
**	st1d	z7\.d, p1, \[x3\]
**	st1h	z0\.h, p0, \[x0\]
**	st1h	z1\.h, p0, \[x1\]
**	st1w	z2\.s, p0, \[x2\]
**	st1d	z3\.d, p0, \[x3\]
**	ret
*/
void __attribute__((noipa))
callee_float (float16_t *x0, float16_t *x1, float32_t *x2, float64_t *x3,
	      svfloat16_t z0, svfloat16_t z1, svfloat32_t z2, svfloat64_t z3,
	      svfloat16_t z4, svfloat16_t z5, svfloat32_t z6, svfloat64_t z7,
	      svfloat16_t z8,
	      svbool_t p0, svbool_t p1, svbool_t p2)
{
  svst1 (p2, x0, z8);
  svst1 (p1, x0, z4);
  svst1 (p1, x1, z5);
  svst1 (p1, x2, z6);
  svst1 (p1, x3, z7);
  svst1 (p0, x0, z0);
  svst1 (p0, x1, z1);
  svst1 (p0, x2, z2);
  svst1 (p0, x3, z3);
}

void __attribute__((noipa))
caller_float (float16_t *x0, float16_t *x1, float32_t *x2, float64_t *x3)
{
  callee_float (x0, x1, x2, x3,
		svdup_f16 (0),
		svdup_f16 (1),
		svdup_f32 (2),
		svdup_f64 (3),
		svdup_f16 (4),
		svdup_f16 (5),
		svdup_f32 (6),
		svdup_f64 (7),
		svdup_f16 (8),
		svptrue_pat_b8 (SV_VL1),
		svptrue_pat_b16 (SV_VL2),
		svptrue_pat_b32 (SV_VL3));
}

/* { dg-final { scan-assembler {\tmov(\tz0\.[bhsd]|i\td0), #0\n} } } */
/* { dg-final { scan-assembler {\tfmov\tz1\.h, #1\.0} } } */
/* { dg-final { scan-assembler {\tfmov\tz2\.s, #2\.0} } } */
/* { dg-final { scan-assembler {\tfmov\tz3\.d, #3\.0} } } */
/* { dg-final { scan-assembler {\tfmov\tz4\.h, #4\.0} } } */
/* { dg-final { scan-assembler {\tfmov\tz5\.h, #5\.0} } } */
/* { dg-final { scan-assembler {\tfmov\tz6\.s, #6\.0} } } */
/* { dg-final { scan-assembler {\tfmov\tz7\.d, #7\.0} } } */
/* { dg-final { scan-assembler {\tmov\tx4, sp\n} } } */
/* { dg-final { scan-assembler {\tfmov\t(z[0-9]+\.h), #8\.0.*\tst1h\t\1, p[0-7], \[(?:x4|sp)\]\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp0\.b, vl1\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp1\.h, vl2\n} } } */
/* { dg-final { scan-assembler {\tptrue\tp2\.s, vl3\n} } } */
