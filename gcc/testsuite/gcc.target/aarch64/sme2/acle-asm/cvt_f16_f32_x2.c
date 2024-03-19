/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sme2_acle.h"

/*
** cvt_z0_z0:
**	fcvt	z0\.h, {z0\.s - z1\.s}
**	ret
*/
TEST_X2_NARROW (cvt_z0_z0, svfloat32x2_t, svfloat16_t,
		z0_res = svcvt_f16_f32_x2 (z0),
		z0_res = svcvt_f16 (z0))

/*
** cvt_z0_z6:
**	fcvt	z0\.h, {z6\.s - z7\.s}
**	ret
*/
TEST_X2_NARROW (cvt_z0_z6, svfloat32x2_t, svfloat16_t,
		z0_res = svcvt_f16_f32_x2 (z6),
		z0_res = svcvt_f16 (z6))

/*
** cvt_z0_z29:
**	mov	[^\n]+
**	mov	[^\n]+
**	fcvt	z0\.h, [^\n]+
**	ret
*/
TEST_X2_NARROW (cvt_z0_z29, svfloat32x2_t, svfloat16_t,
		z0_res = svcvt_f16_f32_x2 (z29),
		z0_res = svcvt_f16 (z29))

/*
** cvt_z5_z0:
**	fcvt	z5\.h, {z0\.s - z1\.s}
**	ret
*/
TEST_X2_NARROW (cvt_z5_z0, svfloat32x2_t, svfloat16_t,
		z5 = svcvt_f16_f32_x2 (z0),
		z5 = svcvt_f16 (z0))

/*
** cvt_z22_z16:
**	fcvt	z22\.h, {z16\.s - z17\.s}
**	ret
*/
TEST_X2_NARROW (cvt_z22_z16, svfloat32x2_t, svfloat16_t,
		z22 = svcvt_f16_f32_x2 (z16),
		z22 = svcvt_f16 (z16))
