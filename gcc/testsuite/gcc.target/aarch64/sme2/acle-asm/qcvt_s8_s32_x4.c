/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** qcvt_z0_z0:
**	sqcvt	z0\.b, {z0\.s - z3\.s}
**	ret
*/
TEST_X4_NARROW (qcvt_z0_z0, svint32x4_t, svint8_t,
		z0_res = svqcvt_s8_s32_x4 (z0),
		z0_res = svqcvt_s8 (z0))

/*
** qcvt_z0_z4:
**	sqcvt	z0\.b, {z4\.s - z7\.s}
**	ret
*/
TEST_X4_NARROW (qcvt_z0_z4, svint32x4_t, svint8_t,
		z0_res = svqcvt_s8_s32_x4 (z4),
		z0_res = svqcvt_s8 (z4))

/*
** qcvt_z0_z21:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqcvt	z0\.b, [^\n]+
**	ret
*/
TEST_X4_NARROW (qcvt_z0_z21, svint32x4_t, svint8_t,
		z0_res = svqcvt_s8_s32_x4 (z21),
		z0_res = svqcvt_s8 (z21))

/*
** qcvt_z25_z26:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqcvt	z25\.b, [^\n]+
**	ret
*/
TEST_X4_NARROW (qcvt_z25_z26, svint32x4_t, svint8_t,
		z25 = svqcvt_s8_s32_x4 (z26),
		z25 = svqcvt_s8 (z26))

/*
** qcvt_z25_z0:
**	sqcvt	z25\.b, {z0\.s - z3\.s}
**	ret
*/
TEST_X4_NARROW (qcvt_z25_z0, svint32x4_t, svint8_t,
		z25 = svqcvt_s8_s32_x4 (z0),
		z25 = svqcvt_s8 (z0))

/*
** qcvt_z22_z16:
**	sqcvt	z22\.b, {z16\.s - z19\.s}
**	ret
*/
TEST_X4_NARROW (qcvt_z22_z16, svint32x4_t, svint8_t,
		z22_res = svqcvt_s8_s32_x4 (z16),
		z22_res = svqcvt_s8 (z16))
