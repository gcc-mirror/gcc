/* { dg-do assemble { target aarch64_asm_sme2p3_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sme2p3_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"
#pragma GCC target "+sme2p3"

/*
** cvtn_z0_z0:
**	fcvtzun	z0\.s, {z0\.d - z1\.d}
**	ret
*/
TEST_X2_NARROW (cvtn_z0_z0, svfloat64x2_t, svuint32_t,
		z0_res = svcvtn_u32_f64_x2 (z0),
		z0_res = svcvtn_u32 (z0))

/*
** cvtn_z0_z6:
**	fcvtzun	z0\.s, {z6\.d - z7\.d}
**	ret
*/
TEST_X2_NARROW (cvtn_z0_z6, svfloat64x2_t, svuint32_t,
		z0_res = svcvtn_u32_f64_x2 (z6),
		z0_res = svcvtn_u32 (z6))

/*
** cvtn_z0_z29:
**	mov	[^\n]+
**	mov	[^\n]+
**	fcvtzun	z0\.s, [^\n]+
**	ret
*/
TEST_X2_NARROW (cvtn_z0_z29, svfloat64x2_t, svuint32_t,
		z0_res = svcvtn_u32_f64_x2 (z29),
		z0_res = svcvtn_u32 (z29))

/*
** cvtn_z5_z0:
**	fcvtzun	z5\.s, {z0\.d - z1\.d}
**	ret
*/
TEST_X2_NARROW (cvtn_z5_z0, svfloat64x2_t, svuint32_t,
		z5 = svcvtn_u32_f64_x2 (z0),
		z5 = svcvtn_u32 (z0))

/*
** cvtn_z22_z16:
**	fcvtzun	z22\.s, {z16\.d - z17\.d}
**	ret
*/
TEST_X2_NARROW (cvtn_z22_z16, svfloat64x2_t, svuint32_t,
		z22 = svcvtn_u32_f64_x2 (z16),
		z22 = svcvtn_u32 (z16))
