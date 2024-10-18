/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** acge_f16_tied:
** (
**	facge	p0\.h, p0/z, z0\.h, z1\.h
** |
**	facle	p0\.h, p0/z, z1\.h, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (acge_f16_tied, svfloat16_t,
		p0 = svacge_f16 (p0, z0, z1),
		p0 = svacge (p0, z0, z1))

/*
** acge_f16_untied:
** (
**	facge	p0\.h, p1/z, z0\.h, z1\.h
** |
**	facle	p0\.h, p1/z, z1\.h, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (acge_f16_untied, svfloat16_t,
		p0 = svacge_f16 (p1, z0, z1),
		p0 = svacge (p1, z0, z1))

/*
** acge_h4_f16:
**	mov	(z[0-9]+\.h), h4
** (
**	facge	p0\.h, p1/z, z0\.h, \1
** |
**	facle	p0\.h, p1/z, \1, z0\.h
** )
**	ret
*/
TEST_COMPARE_ZD (acge_h4_f16, svfloat16_t, float16_t,
		 p0 = svacge_n_f16 (p1, z0, d4),
		 p0 = svacge (p1, z0, d4))

/*
** acge_0_f16:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
** (
**	facge	p0\.h, p1/z, z0\.h, z\1\.h
** |
**	facle	p0\.h, p1/z, \1, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (acge_0_f16, svfloat16_t,
		p0 = svacge_n_f16 (p1, z0, 0),
		p0 = svacge (p1, z0, 0))

/*
** acge_1_f16:
**	fmov	(z[0-9]+\.h), #1\.0(?:e\+0)?
** (
**	facge	p0\.h, p1/z, z0\.h, \1
** |
**	facle	p0\.h, p1/z, \1, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (acge_1_f16, svfloat16_t,
		p0 = svacge_n_f16 (p1, z0, 1),
		p0 = svacge (p1, z0, 1))
