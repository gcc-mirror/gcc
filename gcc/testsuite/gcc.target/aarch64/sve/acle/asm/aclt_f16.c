/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** aclt_f16_tied:
** (
**	facgt	p0\.h, p0/z, z1\.h, z0\.h
** |
**	faclt	p0\.h, p0/z, z0\.h, z1\.h
** )
**	ret
*/
TEST_COMPARE_Z (aclt_f16_tied, svfloat16_t,
		p0 = svaclt_f16 (p0, z0, z1),
		p0 = svaclt (p0, z0, z1))

/*
** aclt_f16_untied:
** (
**	facgt	p0\.h, p1/z, z1\.h, z0\.h
** |
**	faclt	p0\.h, p1/z, z0\.h, z1\.h
** )
**	ret
*/
TEST_COMPARE_Z (aclt_f16_untied, svfloat16_t,
		p0 = svaclt_f16 (p1, z0, z1),
		p0 = svaclt (p1, z0, z1))

/*
** aclt_h4_f16:
**	mov	(z[0-9]+\.h), h4
** (
**	facgt	p0\.h, p1/z, \1, z0\.h
** |
**	faclt	p0\.h, p1/z, z0\.h, \1
** )
**	ret
*/
TEST_COMPARE_ZD (aclt_h4_f16, svfloat16_t, float16_t,
		 p0 = svaclt_n_f16 (p1, z0, d4),
		 p0 = svaclt (p1, z0, d4))

/*
** aclt_0_f16:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
** (
**	facgt	p0\.h, p1/z, \1, z0\.h
** |
**	faclt	p0\.h, p1/z, z0\.h, z\1\.h
** )
**	ret
*/
TEST_COMPARE_Z (aclt_0_f16, svfloat16_t,
		p0 = svaclt_n_f16 (p1, z0, 0),
		p0 = svaclt (p1, z0, 0))

/*
** aclt_1_f16:
**	fmov	(z[0-9]+\.h), #1\.0(?:e\+0)?
** (
**	facgt	p0\.h, p1/z, \1, z0\.h
** |
**	faclt	p0\.h, p1/z, z0\.h, \1
** )
**	ret
*/
TEST_COMPARE_Z (aclt_1_f16, svfloat16_t,
		p0 = svaclt_n_f16 (p1, z0, 1),
		p0 = svaclt (p1, z0, 1))
