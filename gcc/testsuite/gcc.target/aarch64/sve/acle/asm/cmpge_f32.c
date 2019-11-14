/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpge_f32_tied:
** (
**	fcmge	p0\.s, p0/z, z0\.s, z1\.s
** |
**	fcmle	p0\.s, p0/z, z1\.s, z0\.s
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_f32_tied, svfloat32_t,
		p0 = svcmpge_f32 (p0, z0, z1),
		p0 = svcmpge (p0, z0, z1))

/*
** cmpge_f32_untied:
** (
**	fcmge	p0\.s, p1/z, z0\.s, z1\.s
** |
**	fcmle	p0\.s, p1/z, z1\.s, z0\.s
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_f32_untied, svfloat32_t,
		p0 = svcmpge_f32 (p1, z0, z1),
		p0 = svcmpge (p1, z0, z1))

/*
** cmpge_s4_f32:
**	mov	(z[0-9]+\.s), s4
** (
**	fcmge	p0\.s, p1/z, z0\.s, \1
** |
**	fcmle	p0\.s, p1/z, \1, z0\.s
** )
**	ret
*/
TEST_COMPARE_ZD (cmpge_s4_f32, svfloat32_t, float32_t,
		 p0 = svcmpge_n_f32 (p1, z0, d4),
		 p0 = svcmpge (p1, z0, d4))

/*
** cmpge_0_f32:
**	fcmge	p0\.s, p1/z, z0\.s, #0\.0
**	ret
*/
TEST_COMPARE_Z (cmpge_0_f32, svfloat32_t,
		p0 = svcmpge_n_f32 (p1, z0, 0),
		p0 = svcmpge (p1, z0, 0))

/*
** cmpge_1_f32:
**	fmov	(z[0-9]+\.s), #1\.0(?:e\+0)?
** (
**	fcmge	p0\.s, p1/z, z0\.s, \1
** |
**	fcmle	p0\.s, p1/z, \1, z0\.s
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_1_f32, svfloat32_t,
		p0 = svcmpge_n_f32 (p1, z0, 1),
		p0 = svcmpge (p1, z0, 1))
