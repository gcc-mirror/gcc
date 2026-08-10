/* { dg-do assemble { target aarch64_asm_sme2p3_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p3_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+sme2p3"

/*
** qshrn_z0_z0_1:
**	uqshrn	z0\.b, {z0\.h - z1\.h}, #1
**	ret
*/
TEST_X2_NARROW (qshrn_z0_z0_1, svuint16x2_t, svuint8_t,
		z0_res = svqshrn_n_u8_u16_x2 (z0, 1),
		z0_res = svqshrn_u8 (z0, 1))

/*
** qshrn_z0_z6_8:
**	uqshrn	z0\.b, {z6\.h - z7\.h}, #8
**	ret
*/
TEST_X2_NARROW (qshrn_z0_z6_8, svuint16x2_t, svuint8_t,
		z0_res = svqshrn_n_u8_u16_x2 (z6, 8),
		z0_res = svqshrn_u8 (z6, 8))

/*
** qshrn_z0_z29_5:
**	mov	[^\n]+
**	mov	[^\n]+
**	uqshrn	z0\.b, [^\n]+, #5
**	ret
*/
TEST_X2_NARROW (qshrn_z0_z29_5, svuint16x2_t, svuint8_t,
		z0_res = svqshrn_n_u8_u16_x2 (z29, 5),
		z0_res = svqshrn_u8 (z29, 5))

/*
** qshrn_z5_z0_3:
**	uqshrn	z5\.b, {z0\.h - z1\.h}, #3
**	ret
*/
TEST_X2_NARROW (qshrn_z5_z0_3, svuint16x2_t, svuint8_t,
		z5 = svqshrn_n_u8_u16_x2 (z0, 3),
		z5 = svqshrn_u8 (z0, 3))

/*
** qshrn_z22_z16_7:
**	uqshrn	z22\.b, {z16\.h - z17\.h}, #7
**	ret
*/
TEST_X2_NARROW (qshrn_z22_z16_7, svuint16x2_t, svuint8_t,
		z22 = svqshrn_n_u8_u16_x2 (z16, 7),
		z22 = svqshrn_u8 (z16, 7))
