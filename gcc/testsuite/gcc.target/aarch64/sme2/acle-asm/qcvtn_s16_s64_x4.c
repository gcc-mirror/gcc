/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** qcvtn_z0_z0:
**	sqcvtn	z0\.h, {z0\.d - z3\.d}
**	ret
*/
TEST_X4_NARROW (qcvtn_z0_z0, svint64x4_t, svint16_t,
		z0_res = svqcvtn_s16_s64_x4 (z0),
		z0_res = svqcvtn_s16 (z0))

/*
** qcvtn_z0_z4:
**	sqcvtn	z0\.h, {z4\.d - z7\.d}
**	ret
*/
TEST_X4_NARROW (qcvtn_z0_z4, svint64x4_t, svint16_t,
		z0_res = svqcvtn_s16_s64_x4 (z4),
		z0_res = svqcvtn_s16 (z4))

/*
** qcvtn_z0_z21:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqcvtn	z0\.h, [^\n]+
**	ret
*/
TEST_X4_NARROW (qcvtn_z0_z21, svint64x4_t, svint16_t,
		z0_res = svqcvtn_s16_s64_x4 (z21),
		z0_res = svqcvtn_s16 (z21))

/*
** qcvtn_z25_z26:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqcvtn	z25\.h, [^\n]+
**	ret
*/
TEST_X4_NARROW (qcvtn_z25_z26, svint64x4_t, svint16_t,
		z25 = svqcvtn_s16_s64_x4 (z26),
		z25 = svqcvtn_s16 (z26))

/*
** qcvtn_z25_z0:
**	sqcvtn	z25\.h, {z0\.d - z3\.d}
**	ret
*/
TEST_X4_NARROW (qcvtn_z25_z0, svint64x4_t, svint16_t,
		z25 = svqcvtn_s16_s64_x4 (z0),
		z25 = svqcvtn_s16 (z0))

/*
** qcvtn_z22_z16:
**	sqcvtn	z22\.h, {z16\.d - z19\.d}
**	ret
*/
TEST_X4_NARROW (qcvtn_z22_z16, svint64x4_t, svint16_t,
		z22_res = svqcvtn_s16_s64_x4 (z16),
		z22_res = svqcvtn_s16 (z16))
