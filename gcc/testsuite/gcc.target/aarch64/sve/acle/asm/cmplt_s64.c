/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmplt_s64_tied:
** (
**	cmpgt	p0\.d, p0/z, z1\.d, z0\.d
** |
**	cmplt	p0\.d, p0/z, z0\.d, z1\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmplt_s64_tied, svint64_t,
		p0 = svcmplt_s64 (p0, z0, z1),
		p0 = svcmplt (p0, z0, z1))

/*
** cmplt_s64_untied:
** (
**	cmpgt	p0\.d, p1/z, z1\.d, z0\.d
** |
**	cmplt	p0\.d, p1/z, z0\.d, z1\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmplt_s64_untied, svint64_t,
		p0 = svcmplt_s64 (p1, z0, z1),
		p0 = svcmplt (p1, z0, z1))

/*
** cmplt_x0_s64:
**	mov	(z[0-9]+\.d), x0
** (
**	cmpgt	p0\.d, p1/z, \1, z0\.d
** |
**	cmplt	p0\.d, p1/z, z0\.d, \1
** )
**	ret
*/
TEST_COMPARE_ZX (cmplt_x0_s64, svint64_t, int64_t,
		 p0 = svcmplt_n_s64 (p1, z0, x0),
		 p0 = svcmplt (p1, z0, x0))

/*
** cmplt_0_s64:
**	cmplt	p0\.d, p1/z, z0\.d, #0
**	ret
*/
TEST_COMPARE_Z (cmplt_0_s64, svint64_t,
		p0 = svcmplt_n_s64 (p1, z0, 0),
		p0 = svcmplt (p1, z0, 0))

/*
** cmplt_1_s64:
**	cmplt	p0\.d, p1/z, z0\.d, #1
**	ret
*/
TEST_COMPARE_Z (cmplt_1_s64, svint64_t,
		p0 = svcmplt_n_s64 (p1, z0, 1),
		p0 = svcmplt (p1, z0, 1))

/*
** cmplt_15_s64:
**	cmplt	p0\.d, p1/z, z0\.d, #15
**	ret
*/
TEST_COMPARE_Z (cmplt_15_s64, svint64_t,
		p0 = svcmplt_n_s64 (p1, z0, 15),
		p0 = svcmplt (p1, z0, 15))

/*
** cmplt_16_s64:
**	mov	(z[0-9]+\.d), #16
** (
**	cmpgt	p0\.d, p1/z, \1, z0\.d
** |
**	cmplt	p0\.d, p1/z, z0\.d, \1
** )
**	ret
*/
TEST_COMPARE_Z (cmplt_16_s64, svint64_t,
		p0 = svcmplt_n_s64 (p1, z0, 16),
		p0 = svcmplt (p1, z0, 16))

/*
** cmplt_m1_s64:
**	cmplt	p0\.d, p1/z, z0\.d, #-1
**	ret
*/
TEST_COMPARE_Z (cmplt_m1_s64, svint64_t,
		p0 = svcmplt_n_s64 (p1, z0, -1),
		p0 = svcmplt (p1, z0, -1))

/*
** cmplt_m16_s64:
**	cmplt	p0\.d, p1/z, z0\.d, #-16
**	ret
*/
TEST_COMPARE_Z (cmplt_m16_s64, svint64_t,
		p0 = svcmplt_n_s64 (p1, z0, -16),
		p0 = svcmplt (p1, z0, -16))

/*
** cmplt_m17_s64:
**	mov	(z[0-9]+\.d), #-17
** (
**	cmpgt	p0\.d, p1/z, \1, z0\.d
** |
**	cmplt	p0\.d, p1/z, z0\.d, \1
** )
**	ret
*/
TEST_COMPARE_Z (cmplt_m17_s64, svint64_t,
		p0 = svcmplt_n_s64 (p1, z0, -17),
		p0 = svcmplt (p1, z0, -17))
