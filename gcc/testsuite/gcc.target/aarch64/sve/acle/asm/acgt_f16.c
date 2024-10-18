/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** acgt_f16_tied:
** (
**	facgt	p0\.h, p0/z, z0\.h, z1\.h
** |
**	faclt	p0\.h, p0/z, z1\.h, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (acgt_f16_tied, svfloat16_t,
		p0 = svacgt_f16 (p0, z0, z1),
		p0 = svacgt (p0, z0, z1))

/*
** acgt_f16_untied:
** (
**	facgt	p0\.h, p1/z, z0\.h, z1\.h
** |
**	faclt	p0\.h, p1/z, z1\.h, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (acgt_f16_untied, svfloat16_t,
		p0 = svacgt_f16 (p1, z0, z1),
		p0 = svacgt (p1, z0, z1))

/*
** acgt_h4_f16:
**	mov	(z[0-9]+\.h), h4
** (
**	facgt	p0\.h, p1/z, z0\.h, \1
** |
**	faclt	p0\.h, p1/z, \1, z0\.h
** )
**	ret
*/
TEST_COMPARE_ZD (acgt_h4_f16, svfloat16_t, float16_t,
		 p0 = svacgt_n_f16 (p1, z0, d4),
		 p0 = svacgt (p1, z0, d4))

/*
** acgt_0_f16:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
** (
**	facgt	p0\.h, p1/z, z0\.h, z\1\.h
** |
**	faclt	p0\.h, p1/z, \1, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (acgt_0_f16, svfloat16_t,
		p0 = svacgt_n_f16 (p1, z0, 0),
		p0 = svacgt (p1, z0, 0))

/*
** acgt_1_f16:
**	fmov	(z[0-9]+\.h), #1\.0(?:e\+0)?
** (
**	facgt	p0\.h, p1/z, z0\.h, \1
** |
**	faclt	p0\.h, p1/z, \1, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (acgt_1_f16, svfloat16_t,
		p0 = svacgt_n_f16 (p1, z0, 1),
		p0 = svacgt (p1, z0, 1))
