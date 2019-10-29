/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** acgt_f32_tied:
** (
**	facgt	p0\.s, p0/z, z0\.s, z1\.s
** |
**	faclt	p0\.s, p0/z, z1\.s, z0\.s
** )
**	ret
*/
TEST_COMPARE_Z (acgt_f32_tied, svfloat32_t,
		p0 = svacgt_f32 (p0, z0, z1),
		p0 = svacgt (p0, z0, z1))

/*
** acgt_f32_untied:
** (
**	facgt	p0\.s, p1/z, z0\.s, z1\.s
** |
**	faclt	p0\.s, p1/z, z1\.s, z0\.s
** )
**	ret
*/
TEST_COMPARE_Z (acgt_f32_untied, svfloat32_t,
		p0 = svacgt_f32 (p1, z0, z1),
		p0 = svacgt (p1, z0, z1))

/*
** acgt_s4_f32:
**	mov	(z[0-9]+\.s), s4
** (
**	facgt	p0\.s, p1/z, z0\.s, \1
** |
**	faclt	p0\.s, p1/z, \1, z0\.s
** )
**	ret
*/
TEST_COMPARE_ZD (acgt_s4_f32, svfloat32_t, float32_t,
		 p0 = svacgt_n_f32 (p1, z0, d4),
		 p0 = svacgt (p1, z0, d4))

/*
** acgt_0_f32:
**	mov	(z[0-9]+\.s), #0
** (
**	facgt	p0\.s, p1/z, z0\.s, \1
** |
**	faclt	p0\.s, p1/z, \1, z0\.s
** )
**	ret
*/
TEST_COMPARE_Z (acgt_0_f32, svfloat32_t,
		p0 = svacgt_n_f32 (p1, z0, 0),
		p0 = svacgt (p1, z0, 0))

/*
** acgt_1_f32:
**	fmov	(z[0-9]+\.s), #1\.0(?:e\+0)?
** (
**	facgt	p0\.s, p1/z, z0\.s, \1
** |
**	faclt	p0\.s, p1/z, \1, z0\.s
** )
**	ret
*/
TEST_COMPARE_Z (acgt_1_f32, svfloat32_t,
		p0 = svacgt_n_f32 (p1, z0, 1),
		p0 = svacgt (p1, z0, 1))
