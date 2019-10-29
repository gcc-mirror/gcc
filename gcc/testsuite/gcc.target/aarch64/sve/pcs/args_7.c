/* { dg-do compile } */
/* { dg-options "-O -g" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

/*
** callee:
**	...
**	ldr	(x[0-9]+), \[sp\]
**	...
**	ld1b	(z[0-9]+\.b), p[1-3]/z, \[\1\]
**	st1b	\2, p0, \[x0, x7\]
**	ret
*/
void __attribute__((noipa))
callee (int8_t *x0, int x1, int x2, int x3,
	int x4, int x5, svbool_t p0, int x6, int64_t x7,
	svint32x4_t z0, svint32x4_t z4, svint8_t stack)
{
  svst1 (p0, x0 + x7, stack);
}

void __attribute__((noipa))
caller (int8_t *x0, svbool_t p0, svint32x4_t z0, svint32x4_t z4)
{
  callee (x0, 1, 2, 3, 4, 5, p0, 6, 7, z0, z4, svdup_s8 (42));
}

/* { dg-final { scan-assembler {\tmov\t(z[0-9]+\.b), #42\n.*\tst1b\t\1, p[0-7], \[(x[0-9]+)\]\n.*\tstr\t\2, \[sp\]\n} } } */
