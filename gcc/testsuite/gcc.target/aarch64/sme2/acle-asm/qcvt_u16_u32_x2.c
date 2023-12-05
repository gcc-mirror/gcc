/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** qcvt_z0_z0:
**	uqcvt	z0\.h, {z0\.s - z1\.s}
**	ret
*/
TEST_X2_NARROW (qcvt_z0_z0, svuint32x2_t, svuint16_t,
		z0_res = svqcvt_u16_u32_x2 (z0),
		z0_res = svqcvt_u16 (z0))

/*
** qcvt_z0_z6:
**	uqcvt	z0\.h, {z6\.s - z7\.s}
**	ret
*/
TEST_X2_NARROW (qcvt_z0_z6, svuint32x2_t, svuint16_t,
		z0_res = svqcvt_u16_u32_x2 (z6),
		z0_res = svqcvt_u16 (z6))

/*
** qcvt_z0_z29:
**	mov	[^\n]+
**	mov	[^\n]+
**	uqcvt	z0\.h, [^\n]+
**	ret
*/
TEST_X2_NARROW (qcvt_z0_z29, svuint32x2_t, svuint16_t,
		z0_res = svqcvt_u16_u32_x2 (z29),
		z0_res = svqcvt_u16 (z29))

/*
** qcvt_z5_z0:
**	uqcvt	z5\.h, {z0\.s - z1\.s}
**	ret
*/
TEST_X2_NARROW (qcvt_z5_z0, svuint32x2_t, svuint16_t,
		z5 = svqcvt_u16_u32_x2 (z0),
		z5 = svqcvt_u16 (z0))

/*
** qcvt_z22_z16:
**	uqcvt	z22\.h, {z16\.s - z17\.s}
**	ret
*/
TEST_X2_NARROW (qcvt_z22_z16, svuint32x2_t, svuint16_t,
		z22 = svqcvt_u16_u32_x2 (z16),
		z22 = svqcvt_u16 (z16))
