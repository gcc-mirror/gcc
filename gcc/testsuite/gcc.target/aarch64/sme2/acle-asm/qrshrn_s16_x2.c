/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** qrshrn_z0_z0_1:
**	sqrshrn	z0\.h, {z0\.s - z1\.s}, #1
**	ret
*/
TEST_X2_NARROW (qrshrn_z0_z0_1, svint32x2_t, svint16_t,
		z0_res = svqrshrn_n_s16_s32_x2 (z0, 1),
		z0_res = svqrshrn_s16 (z0, 1))

/*
** qrshrn_z0_z6_16:
**	sqrshrn	z0\.h, {z6\.s - z7\.s}, #16
**	ret
*/
TEST_X2_NARROW (qrshrn_z0_z6_16, svint32x2_t, svint16_t,
		z0_res = svqrshrn_n_s16_s32_x2 (z6, 16),
		z0_res = svqrshrn_s16 (z6, 16))

/*
** qrshrn_z0_z29_13:
**	mov	[^\n]+
**	mov	[^\n]+
**	sqrshrn	z0\.h, [^\n]+, #13
**	ret
*/
TEST_X2_NARROW (qrshrn_z0_z29_13, svint32x2_t, svint16_t,
		z0_res = svqrshrn_n_s16_s32_x2 (z29, 13),
		z0_res = svqrshrn_s16 (z29, 13))

/*
** qrshrn_z5_z0_11:
**	sqrshrn	z5\.h, {z0\.s - z1\.s}, #11
**	ret
*/
TEST_X2_NARROW (qrshrn_z5_z0_11, svint32x2_t, svint16_t,
		z5 = svqrshrn_n_s16_s32_x2 (z0, 11),
		z5 = svqrshrn_s16 (z0, 11))

/*
** qrshrn_z22_z16_15:
**	sqrshrn	z22\.h, {z16\.s - z17\.s}, #15
**	ret
*/
TEST_X2_NARROW (qrshrn_z22_z16_15, svint32x2_t, svint16_t,
		z22 = svqrshrn_n_s16_s32_x2 (z16, 15),
		z22 = svqrshrn_s16 (z16, 15))
