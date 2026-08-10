/* { dg-do assemble { target aarch64_asm_sme2p3_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p3_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+sme2p3"

/*
** qshrn_z0_z0_1:
**	sqshrn	z0\.h, {z0\.s - z1\.s}, #1
**	ret
*/
TEST_X2_NARROW (qshrn_z0_z0_1, svint32x2_t, svint16_t,
		z0_res = svqshrn_n_s16_s32_x2 (z0, 1),
		z0_res = svqshrn_s16 (z0, 1))

/*
** qshrn_z0_z6_16:
**	sqshrn	z0\.h, {z6\.s - z7\.s}, #16
**	ret
*/
TEST_X2_NARROW (qshrn_z0_z6_16, svint32x2_t, svint16_t,
		z0_res = svqshrn_n_s16_s32_x2 (z6, 16),
		z0_res = svqshrn_s16 (z6, 16))

/*
** qshrn_z0_z29_13:
**	mov	[^\n]+
**	mov	[^\n]+
**	sqshrn	z0\.h, [^\n]+, #13
**	ret
*/
TEST_X2_NARROW (qshrn_z0_z29_13, svint32x2_t, svint16_t,
		z0_res = svqshrn_n_s16_s32_x2 (z29, 13),
		z0_res = svqshrn_s16 (z29, 13))

/*
** qshrn_z5_z0_11:
**	sqshrn	z5\.h, {z0\.s - z1\.s}, #11
**	ret
*/
TEST_X2_NARROW (qshrn_z5_z0_11, svint32x2_t, svint16_t,
		z5 = svqshrn_n_s16_s32_x2 (z0, 11),
		z5 = svqshrn_s16 (z0, 11))

/*
** qshrn_z22_z16_15:
**	sqshrn	z22\.h, {z16\.s - z17\.s}, #15
**	ret
*/
TEST_X2_NARROW (qshrn_z22_z16_15, svint32x2_t, svint16_t,
		z22 = svqshrn_n_s16_s32_x2 (z16, 15),
		z22 = svqshrn_s16 (z16, 15))
