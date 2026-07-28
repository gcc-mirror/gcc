/* { dg-do assemble { target aarch64_asm_sme2p3_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p3_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+sme2p3"

/*
** cvtn_z0_z0:
**	fcvtzun	z0\.b, {z0\.h - z1\.h}
**	ret
*/
TEST_X2_NARROW (cvtn_z0_z0, svfloat16x2_t, svuint8_t,
		z0_res = svcvtn_u8_f16_x2 (z0),
		z0_res = svcvtn_u8 (z0))

/*
** cvtn_z0_z6:
**	fcvtzun	z0\.b, {z6\.h - z7\.h}
**	ret
*/
TEST_X2_NARROW (cvtn_z0_z6, svfloat16x2_t, svuint8_t,
		z0_res = svcvtn_u8_f16_x2 (z6),
		z0_res = svcvtn_u8 (z6))

/*
** cvtn_z0_z29:
**	mov	[^\n]+
**	mov	[^\n]+
**	fcvtzun	z0\.b, [^\n]+
**	ret
*/
TEST_X2_NARROW (cvtn_z0_z29, svfloat16x2_t, svuint8_t,
		z0_res = svcvtn_u8_f16_x2 (z29),
		z0_res = svcvtn_u8 (z29))

/*
** cvtn_z5_z0:
**	fcvtzun	z5\.b, {z0\.h - z1\.h}
**	ret
*/
TEST_X2_NARROW (cvtn_z5_z0, svfloat16x2_t, svuint8_t,
		z5 = svcvtn_u8_f16_x2 (z0),
		z5 = svcvtn_u8 (z0))

/*
** cvtn_z22_z16:
**	fcvtzun	z22\.b, {z16\.h - z17\.h}
**	ret
*/
TEST_X2_NARROW (cvtn_z22_z16, svfloat16x2_t, svuint8_t,
		z22 = svcvtn_u8_f16_x2 (z16),
		z22 = svcvtn_u8 (z16))
