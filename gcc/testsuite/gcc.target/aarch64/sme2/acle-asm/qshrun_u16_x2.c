/* { dg-do assemble { target aarch64_asm_sme2p3_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p3_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+sme2p3"

/*
** qshrn_z0_z0_1:
**	sqshrun	z0\.h, {z0\.s - z1\.s}, #1
**	ret
*/
TEST_X2_NARROW (qshrn_z0_z0_1, svint32x2_t, svuint16_t,
		z0_res = svqshrun_n_u16_s32_x2 (z0, 1),
		z0_res = svqshrun_u16 (z0, 1))

/*
** qshrn_z0_z6_16:
**	sqshrun	z0\.h, {z6\.s - z7\.s}, #16
**	ret
*/
TEST_X2_NARROW (qshrn_z0_z6_16, svint32x2_t, svuint16_t,
		z0_res = svqshrun_n_u16_s32_x2 (z6, 16),
		z0_res = svqshrun_u16 (z6, 16))

/*
** qshrn_z0_z29_13:
**	mov	[^\n]+
**	mov	[^\n]+
**	sqshrun	z0\.h, [^\n]+, #13
**	ret
*/
TEST_X2_NARROW (qshrn_z0_z29_13, svint32x2_t, svuint16_t,
		z0_res = svqshrun_n_u16_s32_x2 (z29, 13),
		z0_res = svqshrun_u16 (z29, 13))

/*
** qshrn_z5_z0_11:
**	sqshrun	z5\.h, {z0\.s - z1\.s}, #11
**	ret
*/
TEST_X2_NARROW (qshrn_z5_z0_11, svint32x2_t, svuint16_t,
		z5 = svqshrun_n_u16_s32_x2 (z0, 11),
		z5 = svqshrun_u16 (z0, 11))

/*
** qshrn_z22_z16_15:
**	sqshrun	z22\.h, {z16\.s - z17\.s}, #15
**	ret
*/
TEST_X2_NARROW (qshrn_z22_z16_15, svint32x2_t, svuint16_t,
		z22 = svqshrun_n_u16_s32_x2 (z16, 15),
		z22 = svqshrun_u16 (z16, 15))
