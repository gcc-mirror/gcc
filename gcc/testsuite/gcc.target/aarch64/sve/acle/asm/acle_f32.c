/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** acle_f32_tied:
** (
**	facge	p0\.s, p0/z, z1\.s, z0\.s
** |
**	facle	p0\.s, p0/z, z0\.s, z1\.s
** )
**	ret
*/
TEST_COMPARE_Z (acle_f32_tied, svfloat32_t,
		p0 = svacle_f32 (p0, z0, z1),
		p0 = svacle (p0, z0, z1))

/*
** acle_f32_untied:
** (
**	facge	p0\.s, p1/z, z1\.s, z0\.s
** |
**	facle	p0\.s, p1/z, z0\.s, z1\.s
** )
**	ret
*/
TEST_COMPARE_Z (acle_f32_untied, svfloat32_t,
		p0 = svacle_f32 (p1, z0, z1),
		p0 = svacle (p1, z0, z1))

/*
** acle_s4_f32:
**	mov	(z[0-9]+\.s), s4
** (
**	facge	p0\.s, p1/z, \1, z0\.s
** |
**	facle	p0\.s, p1/z, z0\.s, \1
** )
**	ret
*/
TEST_COMPARE_ZD (acle_s4_f32, svfloat32_t, float32_t,
		 p0 = svacle_n_f32 (p1, z0, d4),
		 p0 = svacle (p1, z0, d4))

/*
** acle_0_f32:
**	mov	(z[0-9]+\.s), #0
** (
**	facge	p0\.s, p1/z, \1, z0\.s
** |
**	facle	p0\.s, p1/z, z0\.s, \1
** )
**	ret
*/
TEST_COMPARE_Z (acle_0_f32, svfloat32_t,
		p0 = svacle_n_f32 (p1, z0, 0),
		p0 = svacle (p1, z0, 0))

/*
** acle_1_f32:
**	fmov	(z[0-9]+\.s), #1\.0(?:e\+0)?
** (
**	facge	p0\.s, p1/z, \1, z0\.s
** |
**	facle	p0\.s, p1/z, z0\.s, \1
** )
**	ret
*/
TEST_COMPARE_Z (acle_1_f32, svfloat32_t,
		p0 = svacle_n_f32 (p1, z0, 1),
		p0 = svacle (p1, z0, 1))
