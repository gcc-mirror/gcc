/* { dg-do compile { target lp64 } } */
/* { dg-options "-O -g" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

/*
** callee:
**	ldr	(x[0-9]+), \[sp, 8\]
**	ldr	p0, \[\1\]
**	ret
*/
svbool_t __attribute__((noipa))
callee (svint64x4_t z0, svint16x4_t z4,
	svint64_t stack1, svint32_t stack2,
	svint16_t stack3, svint8_t stack4,
	svuint64_t stack5, svuint32_t stack6,
	svuint16_t stack7, svuint8_t stack8,
	svbool_t p0, svbool_t p1, svbool_t p2, svbool_t p3,
	svbool_t stack9, svbool_t stack10)
{
  return stack10;
}

uint64_t __attribute__((noipa))
caller (int64_t *x0, int16_t *x1, svbool_t p0)
{
  svbool_t res;
  res = callee (svld4 (p0, x0),
		svld4 (p0, x1),
		svdup_s64 (1),
		svdup_s32 (2),
		svdup_s16 (3),
		svdup_s8 (4),
		svdup_u64 (5),
		svdup_u32 (6),
		svdup_u16 (7),
		svdup_u8 (8),
		svptrue_pat_b8 (SV_VL5),
		svptrue_pat_b16 (SV_VL6),
		svptrue_pat_b32 (SV_VL7),
		svptrue_pat_b64 (SV_VL8),
		svptrue_pat_b8 (SV_MUL3),
		svptrue_pat_b16 (SV_MUL3));
  return svcntp_b8 (res, res);
}

/* { dg-final { scan-assembler {\tptrue\t(p[0-9]+)\.b, mul3\n\tstr\t\1, \[(x[0-9]+)\]\n.*\tstr\t\2, \[sp\]\n} } } */
/* { dg-final { scan-assembler {\tptrue\t(p[0-9]+)\.h, mul3\n\tstr\t\1, \[(x[0-9]+)\]\n.*\tstr\t\2, \[sp, 8\]\n} } } */
