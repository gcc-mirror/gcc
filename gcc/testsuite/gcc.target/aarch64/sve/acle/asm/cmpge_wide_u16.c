/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpge_wide_u16_tied:
**	cmphs	p0\.h, p0/z, z0\.h, z1\.d
**	ret
*/
TEST_COMPARE_DUAL_Z (cmpge_wide_u16_tied, svuint16_t, svuint64_t,
		     p0 = svcmpge_wide_u16 (p0, z0, z1),
		     p0 = svcmpge_wide (p0, z0, z1))

/*
** cmpge_wide_u16_untied:
**	cmphs	p0\.h, p1/z, z0\.h, z1\.d
**	ret
*/
TEST_COMPARE_DUAL_Z (cmpge_wide_u16_untied, svuint16_t, svuint64_t,
		     p0 = svcmpge_wide_u16 (p1, z0, z1),
		     p0 = svcmpge_wide (p1, z0, z1))

/*
** cmpge_wide_x0_u16:
**	mov	(z[0-9]+\.d), x0
**	cmphs	p0\.h, p1/z, z0\.h, \1
**	ret
*/
TEST_COMPARE_ZX (cmpge_wide_x0_u16, svuint16_t, uint64_t,
		 p0 = svcmpge_wide_n_u16 (p1, z0, x0),
		 p0 = svcmpge_wide (p1, z0, x0))

/*
** cmpge_wide_0_u16:
**	cmphs	p0\.h, p1/z, z0\.h, #0
**	ret
*/
TEST_COMPARE_Z (cmpge_wide_0_u16, svuint16_t,
		p0 = svcmpge_wide_n_u16 (p1, z0, 0),
		p0 = svcmpge_wide (p1, z0, 0))

/*
** cmpge_wide_1_u16:
**	cmphs	p0\.h, p1/z, z0\.h, #1
**	ret
*/
TEST_COMPARE_Z (cmpge_wide_1_u16, svuint16_t,
		p0 = svcmpge_wide_n_u16 (p1, z0, 1),
		p0 = svcmpge_wide (p1, z0, 1))

/*
** cmpge_wide_15_u16:
**	cmphs	p0\.h, p1/z, z0\.h, #15
**	ret
*/
TEST_COMPARE_Z (cmpge_wide_15_u16, svuint16_t,
		p0 = svcmpge_wide_n_u16 (p1, z0, 15),
		p0 = svcmpge_wide (p1, z0, 15))

/*
** cmpge_wide_16_u16:
**	cmphs	p0\.h, p1/z, z0\.h, #16
**	ret
*/
TEST_COMPARE_Z (cmpge_wide_16_u16, svuint16_t,
		p0 = svcmpge_wide_n_u16 (p1, z0, 16),
		p0 = svcmpge_wide (p1, z0, 16))

/*
** cmpge_wide_127_u16:
**	cmphs	p0\.h, p1/z, z0\.h, #127
**	ret
*/
TEST_COMPARE_Z (cmpge_wide_127_u16, svuint16_t,
		p0 = svcmpge_wide_n_u16 (p1, z0, 127),
		p0 = svcmpge_wide (p1, z0, 127))

/*
** cmpge_wide_128_u16:
**	mov	(z[0-9]+\.d), #128
**	cmphs	p0\.h, p1/z, z0\.h, \1
**	ret
*/
TEST_COMPARE_Z (cmpge_wide_128_u16, svuint16_t,
		p0 = svcmpge_wide_n_u16 (p1, z0, 128),
		p0 = svcmpge_wide (p1, z0, 128))

/*
** cmpge_wide_m1_u16:
**	mov	(z[0-9]+)\.b, #-1
**	cmphs	p0\.h, p1/z, z0\.h, \1\.d
**	ret
*/
TEST_COMPARE_Z (cmpge_wide_m1_u16, svuint16_t,
		p0 = svcmpge_wide_n_u16 (p1, z0, -1),
		p0 = svcmpge_wide (p1, z0, -1))
