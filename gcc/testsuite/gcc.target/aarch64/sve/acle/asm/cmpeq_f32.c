/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpeq_f32_tied:
**	fcmeq	p0\.s, p0/z, (z0\.s, z1\.s|z1\.s, z0\.s)
**	ret
*/
TEST_COMPARE_Z (cmpeq_f32_tied, svfloat32_t,
		p0 = svcmpeq_f32 (p0, z0, z1),
		p0 = svcmpeq (p0, z0, z1))

/*
** cmpeq_f32_untied:
**	fcmeq	p0\.s, p1/z, (z0\.s, z1\.s|z1\.s, z0\.s)
**	ret
*/
TEST_COMPARE_Z (cmpeq_f32_untied, svfloat32_t,
		p0 = svcmpeq_f32 (p1, z0, z1),
		p0 = svcmpeq (p1, z0, z1))

/*
** cmpeq_s4_f32:
**	mov	(z[0-9]+\.s), s4
**	fcmeq	p0\.s, p1/z, (z0\.s, \1|\1, z0\.s)
**	ret
*/
TEST_COMPARE_ZD (cmpeq_s4_f32, svfloat32_t, float32_t,
		 p0 = svcmpeq_n_f32 (p1, z0, d4),
		 p0 = svcmpeq (p1, z0, d4))

/*
** cmpeq_0_f32:
**	fcmeq	p0\.s, p1/z, z0\.s, #0\.0
**	ret
*/
TEST_COMPARE_Z (cmpeq_0_f32, svfloat32_t,
		p0 = svcmpeq_n_f32 (p1, z0, 0),
		p0 = svcmpeq (p1, z0, 0))

/*
** cmpeq_1_f32:
**	fmov	(z[0-9]+\.s), #1\.0(?:e\+0)?
**	fcmeq	p0\.s, p1/z, (z0\.s, \1|\1, z0\.s)
**	ret
*/
TEST_COMPARE_Z (cmpeq_1_f32, svfloat32_t,
		p0 = svcmpeq_n_f32 (p1, z0, 1),
		p0 = svcmpeq (p1, z0, 1))
