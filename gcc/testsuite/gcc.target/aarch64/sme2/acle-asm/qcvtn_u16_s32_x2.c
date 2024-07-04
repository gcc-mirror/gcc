/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** qcvtn_z0_z0:
**	sqcvtun	z0\.h, {z0\.s - z1\.s}
**	ret
*/
TEST_X2_NARROW (qcvtn_z0_z0, svint32x2_t, svuint16_t,
		z0_res = svqcvtn_u16_s32_x2 (z0),
		z0_res = svqcvtn_u16 (z0))

/*
** qcvtn_z0_z6:
**	sqcvtun	z0\.h, {z6\.s - z7\.s}
**	ret
*/
TEST_X2_NARROW (qcvtn_z0_z6, svint32x2_t, svuint16_t,
		z0_res = svqcvtn_u16_s32_x2 (z6),
		z0_res = svqcvtn_u16 (z6))

/*
** qcvtn_z0_z29:
**	mov	[^\n]+
**	mov	[^\n]+
**	sqcvtun	z0\.h, [^\n]+
**	ret
*/
TEST_X2_NARROW (qcvtn_z0_z29, svint32x2_t, svuint16_t,
		z0_res = svqcvtn_u16_s32_x2 (z29),
		z0_res = svqcvtn_u16 (z29))

/*
** qcvtn_z5_z0:
**	sqcvtun	z5\.h, {z0\.s - z1\.s}
**	ret
*/
TEST_X2_NARROW (qcvtn_z5_z0, svint32x2_t, svuint16_t,
		z5 = svqcvtn_u16_s32_x2 (z0),
		z5 = svqcvtn_u16 (z0))

/*
** qcvtn_z22_z16:
**	sqcvtun	z22\.h, {z16\.s - z17\.s}
**	ret
*/
TEST_X2_NARROW (qcvtn_z22_z16, svint32x2_t, svuint16_t,
		z22 = svqcvtn_u16_s32_x2 (z16),
		z22 = svqcvtn_u16 (z16))
