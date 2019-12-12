/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmplt_f32_tied:
** (
**	fcmgt	p0\.s, p0/z, z1\.s, z0\.s
** |
**	fcmlt	p0\.s, p0/z, z0\.s, z1\.s
** )
**	ret
*/
TEST_COMPARE_Z (cmplt_f32_tied, svfloat32_t,
		p0 = svcmplt_f32 (p0, z0, z1),
		p0 = svcmplt (p0, z0, z1))

/*
** cmplt_f32_untied:
** (
**	fcmgt	p0\.s, p1/z, z1\.s, z0\.s
** |
**	fcmlt	p0\.s, p1/z, z0\.s, z1\.s
** )
**	ret
*/
TEST_COMPARE_Z (cmplt_f32_untied, svfloat32_t,
		p0 = svcmplt_f32 (p1, z0, z1),
		p0 = svcmplt (p1, z0, z1))

/*
** cmplt_s4_f32:
**	mov	(z[0-9]+\.s), s4
** (
**	fcmgt	p0\.s, p1/z, \1, z0\.s
** |
**	fcmlt	p0\.s, p1/z, z0\.s, \1
** )
**	ret
*/
TEST_COMPARE_ZD (cmplt_s4_f32, svfloat32_t, float32_t,
		 p0 = svcmplt_n_f32 (p1, z0, d4),
		 p0 = svcmplt (p1, z0, d4))

/*
** cmplt_0_f32:
**	fcmlt	p0\.s, p1/z, z0\.s, #0\.0
**	ret
*/
TEST_COMPARE_Z (cmplt_0_f32, svfloat32_t,
		p0 = svcmplt_n_f32 (p1, z0, 0),
		p0 = svcmplt (p1, z0, 0))

/*
** cmplt_1_f32:
**	fmov	(z[0-9]+\.s), #1\.0(?:e\+0)?
** (
**	fcmgt	p0\.s, p1/z, \1, z0\.s
** |
**	fcmlt	p0\.s, p1/z, z0\.s, \1
** )
**	ret
*/
TEST_COMPARE_Z (cmplt_1_f32, svfloat32_t,
		p0 = svcmplt_n_f32 (p1, z0, 1),
		p0 = svcmplt (p1, z0, 1))
