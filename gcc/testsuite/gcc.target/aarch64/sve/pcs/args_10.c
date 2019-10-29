/* { dg-do compile } */
/* { dg-options "-O -fno-stack-clash-protection -g" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

/*
** callee:
**	fadd	s0, (s0, s6|s6, s0)
**	ret
*/
float __attribute__((noipa))
callee (float s0, double d1, svfloat32x4_t z2, svfloat64x4_t stack1,
	float s6, double d7)
{
  return s0 + s6;
}

float __attribute__((noipa))
caller (float32_t *x0, float64_t *x1)
{
  return callee (0.0f, 1.0,
		 svld4 (svptrue_b8 (), x0),
		 svld4 (svptrue_b8 (), x1),
		 6.0f, 7.0);
}

/* { dg-final { scan-assembler {\tld4w\t{z2\.s - z5\.s}, p[0-7]/z, \[x0\]\n} } } */
/* { dg-final { scan-assembler {\tld4d\t{z[0-9]+\.d - z[0-9]+\.d}, p[0-7]/z, \[x1\]\n} } } */
/* { dg-final { scan-assembler {\tmovi\tv0\.[24]s, #0\n} } } */
/* { dg-final { scan-assembler {\tfmov\td1, #?1\.0} } } */
/* { dg-final { scan-assembler {\tfmov\ts6, #?6\.0} } } */
/* { dg-final { scan-assembler {\tfmov\td7, #?7\.0} } } */
