/* { dg-do assemble { target aarch64_asm_sme2p3_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p3_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+sme2p3"

/*
** cvtn_z0_z0:
**	fcvtzsn	z0\.h, {z0\.s - z1\.s}
**	ret
*/
TEST_X2_NARROW (cvtn_z0_z0, svfloat32x2_t, svint16_t,
		z0_res = svcvtn_s16_f32_x2 (z0),
		z0_res = svcvtn_s16 (z0))

/*
** cvtn_z0_z6:
**	fcvtzsn	z0\.h, {z6\.s - z7\.s}
**	ret
*/
TEST_X2_NARROW (cvtn_z0_z6, svfloat32x2_t, svint16_t,
		z0_res = svcvtn_s16_f32_x2 (z6),
		z0_res = svcvtn_s16 (z6))

/*
** cvtn_z0_z29:
**	mov	[^\n]+
**	mov	[^\n]+
**	fcvtzsn	z0\.h, [^\n]+
**	ret
*/
TEST_X2_NARROW (cvtn_z0_z29, svfloat32x2_t, svint16_t,
		z0_res = svcvtn_s16_f32_x2 (z29),
		z0_res = svcvtn_s16 (z29))

/*
** cvtn_z5_z0:
**	fcvtzsn	z5\.h, {z0\.s - z1\.s}
**	ret
*/
TEST_X2_NARROW (cvtn_z5_z0, svfloat32x2_t, svint16_t,
		z5 = svcvtn_s16_f32_x2 (z0),
		z5 = svcvtn_s16 (z0))

/*
** cvtn_z22_z16:
**	fcvtzsn	z22\.h, {z16\.s - z17\.s}
**	ret
*/
TEST_X2_NARROW (cvtn_z22_z16, svfloat32x2_t, svint16_t,
		z22 = svcvtn_s16_f32_x2 (z16),
		z22 = svcvtn_s16 (z16))
