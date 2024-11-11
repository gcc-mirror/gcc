/* { dg-do assemble { target aarch64_asm_sve2p1_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2p1"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** qrshrun_z0_z0_1:
**	sqrshrun	z0\.h, {z0\.s - z1\.s}, #1
**	ret
*/
TEST_X2_NARROW (qrshrun_z0_z0_1, svint32x2_t, svuint16_t,
		z0_res = svqrshrun_n_u16_s32_x2 (z0, 1),
		z0_res = svqrshrun_u16 (z0, 1))

/*
** qrshrun_z0_z6_16:
**	sqrshrun	z0\.h, {z6\.s - z7\.s}, #16
**	ret
*/
TEST_X2_NARROW (qrshrun_z0_z6_16, svint32x2_t, svuint16_t,
		z0_res = svqrshrun_n_u16_s32_x2 (z6, 16),
		z0_res = svqrshrun_u16 (z6, 16))

/*
** qrshrun_z0_z29_13:
**	mov	[^\n]+
**	mov	[^\n]+
**	sqrshrun	z0\.h, [^\n]+, #13
**	ret
*/
TEST_X2_NARROW (qrshrun_z0_z29_13, svint32x2_t, svuint16_t,
		z0_res = svqrshrun_n_u16_s32_x2 (z29, 13),
		z0_res = svqrshrun_u16 (z29, 13))

/*
** qrshrun_z5_z0_11:
**	sqrshrun	z5\.h, {z0\.s - z1\.s}, #11
**	ret
*/
TEST_X2_NARROW (qrshrun_z5_z0_11, svint32x2_t, svuint16_t,
		z5 = svqrshrun_n_u16_s32_x2 (z0, 11),
		z5 = svqrshrun_u16 (z0, 11))

/*
** qrshrun_z22_z16_15:
**	sqrshrun	z22\.h, {z16\.s - z17\.s}, #15
**	ret
*/
TEST_X2_NARROW (qrshrun_z22_z16_15, svint32x2_t, svuint16_t,
		z22 = svqrshrun_n_u16_s32_x2 (z16, 15),
		z22 = svqrshrun_u16 (z16, 15))
