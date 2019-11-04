/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpgt_s64_tied:
** (
**	cmpgt	p0\.d, p0/z, z0\.d, z1\.d
** |
**	cmplt	p0\.d, p0/z, z1\.d, z0\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_s64_tied, svint64_t,
		p0 = svcmpgt_s64 (p0, z0, z1),
		p0 = svcmpgt (p0, z0, z1))

/*
** cmpgt_s64_untied:
** (
**	cmpgt	p0\.d, p1/z, z0\.d, z1\.d
** |
**	cmplt	p0\.d, p1/z, z1\.d, z0\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_s64_untied, svint64_t,
		p0 = svcmpgt_s64 (p1, z0, z1),
		p0 = svcmpgt (p1, z0, z1))

/*
** cmpgt_x0_s64:
**	mov	(z[0-9]+\.d), x0
** (
**	cmpgt	p0\.d, p1/z, z0\.d, \1
** |
**	cmplt	p0\.d, p1/z, \1, z0\.d
** )
**	ret
*/
TEST_COMPARE_ZX (cmpgt_x0_s64, svint64_t, int64_t,
		 p0 = svcmpgt_n_s64 (p1, z0, x0),
		 p0 = svcmpgt (p1, z0, x0))

/*
** cmpgt_0_s64:
**	cmpgt	p0\.d, p1/z, z0\.d, #0
**	ret
*/
TEST_COMPARE_Z (cmpgt_0_s64, svint64_t,
		p0 = svcmpgt_n_s64 (p1, z0, 0),
		p0 = svcmpgt (p1, z0, 0))

/*
** cmpgt_1_s64:
**	cmpgt	p0\.d, p1/z, z0\.d, #1
**	ret
*/
TEST_COMPARE_Z (cmpgt_1_s64, svint64_t,
		p0 = svcmpgt_n_s64 (p1, z0, 1),
		p0 = svcmpgt (p1, z0, 1))

/*
** cmpgt_15_s64:
**	cmpgt	p0\.d, p1/z, z0\.d, #15
**	ret
*/
TEST_COMPARE_Z (cmpgt_15_s64, svint64_t,
		p0 = svcmpgt_n_s64 (p1, z0, 15),
		p0 = svcmpgt (p1, z0, 15))

/*
** cmpgt_16_s64:
**	mov	(z[0-9]+\.d), #16
** (
**	cmpgt	p0\.d, p1/z, z0\.d, \1
** |
**	cmplt	p0\.d, p1/z, \1, z0\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_16_s64, svint64_t,
		p0 = svcmpgt_n_s64 (p1, z0, 16),
		p0 = svcmpgt (p1, z0, 16))

/*
** cmpgt_m1_s64:
**	cmpgt	p0\.d, p1/z, z0\.d, #-1
**	ret
*/
TEST_COMPARE_Z (cmpgt_m1_s64, svint64_t,
		p0 = svcmpgt_n_s64 (p1, z0, -1),
		p0 = svcmpgt (p1, z0, -1))

/*
** cmpgt_m16_s64:
**	cmpgt	p0\.d, p1/z, z0\.d, #-16
**	ret
*/
TEST_COMPARE_Z (cmpgt_m16_s64, svint64_t,
		p0 = svcmpgt_n_s64 (p1, z0, -16),
		p0 = svcmpgt (p1, z0, -16))

/*
** cmpgt_m17_s64:
**	mov	(z[0-9]+\.d), #-17
** (
**	cmpgt	p0\.d, p1/z, z0\.d, \1
** |
**	cmplt	p0\.d, p1/z, \1, z0\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_m17_s64, svint64_t,
		p0 = svcmpgt_n_s64 (p1, z0, -17),
		p0 = svcmpgt (p1, z0, -17))
