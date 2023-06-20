/* { dg-do compile { target lp64 } } */
/* { dg-options "-O -g" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

/*
** callee:
**	ptrue	(p[1-3])\.b, all
**	ld1b	(z[0-9]+\.b), \1/z, \[x4\]
**	st1b	\2, p0, \[x0, x7\]
**	ret
*/
void __attribute__((noipa))
callee (int8_t *x0, int x1, int x2, int x3,
	svint32x4_t z0, svint32x4_t z4, svint8_t stack,
	int x5, svbool_t p0, int x6, int64_t x7)
{
  svst1 (p0, x0 + x7, stack);
}

void __attribute__((noipa))
caller (int8_t *x0, svbool_t p0, svint32x4_t z0, svint32x4_t z4)
{
  callee (x0, 1, 2, 3, z0, z4, svdup_s8 (42), 5, p0, 6, 7);
}

/* { dg-final { scan-assembler {\tmov\t(z[0-9]+\.b), #42\n.*\tst1b\t\1, p[0-7], \[(?:x4|sp)\]\n} } } */
/* { dg-final { scan-assembler {\tmov\tx4, sp.*\tst1b\tz[0-9]+\.b, p[0-7], \[(?:x4|sp)\]\n} } } */
