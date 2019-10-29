/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpge_s64_tied:
** (
**	cmpge	p0\.d, p0/z, z0\.d, z1\.d
** |
**	cmple	p0\.d, p0/z, z1\.d, z0\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_s64_tied, svint64_t,
		p0 = svcmpge_s64 (p0, z0, z1),
		p0 = svcmpge (p0, z0, z1))

/*
** cmpge_s64_untied:
** (
**	cmpge	p0\.d, p1/z, z0\.d, z1\.d
** |
**	cmple	p0\.d, p1/z, z1\.d, z0\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_s64_untied, svint64_t,
		p0 = svcmpge_s64 (p1, z0, z1),
		p0 = svcmpge (p1, z0, z1))

/*
** cmpge_x0_s64:
**	mov	(z[0-9]+\.d), x0
** (
**	cmpge	p0\.d, p1/z, z0\.d, \1
** |
**	cmple	p0\.d, p1/z, \1, z0\.d
** )
**	ret
*/
TEST_COMPARE_ZX (cmpge_x0_s64, svint64_t, int64_t,
		 p0 = svcmpge_n_s64 (p1, z0, x0),
		 p0 = svcmpge (p1, z0, x0))

/*
** cmpge_0_s64:
**	cmpge	p0\.d, p1/z, z0\.d, #0
**	ret
*/
TEST_COMPARE_Z (cmpge_0_s64, svint64_t,
		p0 = svcmpge_n_s64 (p1, z0, 0),
		p0 = svcmpge (p1, z0, 0))

/*
** cmpge_1_s64:
**	cmpge	p0\.d, p1/z, z0\.d, #1
**	ret
*/
TEST_COMPARE_Z (cmpge_1_s64, svint64_t,
		p0 = svcmpge_n_s64 (p1, z0, 1),
		p0 = svcmpge (p1, z0, 1))

/*
** cmpge_15_s64:
**	cmpge	p0\.d, p1/z, z0\.d, #15
**	ret
*/
TEST_COMPARE_Z (cmpge_15_s64, svint64_t,
		p0 = svcmpge_n_s64 (p1, z0, 15),
		p0 = svcmpge (p1, z0, 15))

/*
** cmpge_16_s64:
**	mov	(z[0-9]+\.d), #16
** (
**	cmpge	p0\.d, p1/z, z0\.d, \1
** |
**	cmple	p0\.d, p1/z, \1, z0\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_16_s64, svint64_t,
		p0 = svcmpge_n_s64 (p1, z0, 16),
		p0 = svcmpge (p1, z0, 16))

/*
** cmpge_m1_s64:
**	cmpge	p0\.d, p1/z, z0\.d, #-1
**	ret
*/
TEST_COMPARE_Z (cmpge_m1_s64, svint64_t,
		p0 = svcmpge_n_s64 (p1, z0, -1),
		p0 = svcmpge (p1, z0, -1))

/*
** cmpge_m16_s64:
**	cmpge	p0\.d, p1/z, z0\.d, #-16
**	ret
*/
TEST_COMPARE_Z (cmpge_m16_s64, svint64_t,
		p0 = svcmpge_n_s64 (p1, z0, -16),
		p0 = svcmpge (p1, z0, -16))

/*
** cmpge_m17_s64:
**	mov	(z[0-9]+\.d), #-17
** (
**	cmpge	p0\.d, p1/z, z0\.d, \1
** |
**	cmple	p0\.d, p1/z, \1, z0\.d
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_m17_s64, svint64_t,
		p0 = svcmpge_n_s64 (p1, z0, -17),
		p0 = svcmpge (p1, z0, -17))
