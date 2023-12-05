/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** qrshrn_z0_z0_1:
**	sqrshrn	z0\.h, {z0\.d - z3\.d}, #1
**	ret
*/
TEST_X4_NARROW (qrshrn_z0_z0_1, svint64x4_t, svint16_t,
		z0_res = svqrshrn_n_s16_s64_x4 (z0, 1),
		z0_res = svqrshrn_s16 (z0, 1))

/*
** qrshrn_z0_z4_64:
**	sqrshrn	z0\.h, {z4\.d - z7\.d}, #64
**	ret
*/
TEST_X4_NARROW (qrshrn_z0_z4_64, svint64x4_t, svint16_t,
		z0_res = svqrshrn_n_s16_s64_x4 (z4, 64),
		z0_res = svqrshrn_s16 (z4, 64))

/*
** qrshrn_z0_z21_33:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqrshrn	z0\.h, [^\n]+, #33
**	ret
*/
TEST_X4_NARROW (qrshrn_z0_z21_33, svint64x4_t, svint16_t,
		z0_res = svqrshrn_n_s16_s64_x4 (z21, 33),
		z0_res = svqrshrn_s16 (z21, 33))

/*
** qrshrn_z25_z26_12:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	sqrshrn	z25\.h, [^\n]+, #12
**	ret
*/
TEST_X4_NARROW (qrshrn_z25_z26_12, svint64x4_t, svint16_t,
		z25 = svqrshrn_n_s16_s64_x4 (z26, 12),
		z25 = svqrshrn_s16 (z26, 12))

/*
** qrshrn_z25_z0_32:
**	sqrshrn	z25\.h, {z0\.d - z3\.d}, #32
**	ret
*/
TEST_X4_NARROW (qrshrn_z25_z0_32, svint64x4_t, svint16_t,
		z25 = svqrshrn_n_s16_s64_x4 (z0, 32),
		z25 = svqrshrn_s16 (z0, 32))

/*
** qrshrn_z22_z16_63:
**	sqrshrn	z22\.h, {z16\.d - z19\.d}, #63
**	ret
*/
TEST_X4_NARROW (qrshrn_z22_z16_63, svint64x4_t, svint16_t,
		z22_res = svqrshrn_n_s16_s64_x4 (z16, 63),
		z22_res = svqrshrn_s16 (z16, 63))
