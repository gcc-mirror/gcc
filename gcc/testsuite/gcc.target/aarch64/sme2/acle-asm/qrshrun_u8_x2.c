/* { dg-do assemble { target aarch64_asm_sme2p3_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p3_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+sme2p3"

/*
** qrshrun_z0_z0_1:
**	sqrshrun	z0\.b, {z0\.h - z1\.h}, #1
**	ret
*/
TEST_X2_NARROW (qrshrun_z0_z0_1, svint16x2_t, svuint8_t,
		z0_res = svqrshrun_n_u8_s16_x2 (z0, 1),
		z0_res = svqrshrun_u8 (z0, 1))

/*
** qrshrun_z0_z6_8:
**	sqrshrun	z0\.b, {z6\.h - z7\.h}, #8
**	ret
*/
TEST_X2_NARROW (qrshrun_z0_z6_8, svint16x2_t, svuint8_t,
		z0_res = svqrshrun_n_u8_s16_x2 (z6, 8),
		z0_res = svqrshrun_u8 (z6, 8))

/*
** qrshrun_z0_z29_5:
**	mov	[^\n]+
**	mov	[^\n]+
**	sqrshrun	z0\.b, [^\n]+, #5
**	ret
*/
TEST_X2_NARROW (qrshrun_z0_z29_5, svint16x2_t, svuint8_t,
		z0_res = svqrshrun_n_u8_s16_x2 (z29, 5),
		z0_res = svqrshrun_u8 (z29, 5))

/*
** qrshrun_z5_z0_3:
**	sqrshrun	z5\.b, {z0\.h - z1\.h}, #3
**	ret
*/
TEST_X2_NARROW (qrshrun_z5_z0_3, svint16x2_t, svuint8_t,
		z5 = svqrshrun_n_u8_s16_x2 (z0, 3),
		z5 = svqrshrun_u8 (z0, 3))

/*
** qrshrun_z22_z16_7:
**	sqrshrun	z22\.b, {z16\.h - z17\.h}, #7
**	ret
*/
TEST_X2_NARROW (qrshrun_z22_z16_7, svint16x2_t, svuint8_t,
		z22 = svqrshrun_n_u8_s16_x2 (z16, 7),
		z22 = svqrshrun_u8 (z16, 7))
