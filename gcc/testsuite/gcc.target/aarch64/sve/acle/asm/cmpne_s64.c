/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpne_s64_tied:
**	cmpne	p0\.d, p0/z, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_COMPARE_Z (cmpne_s64_tied, svint64_t,
		p0 = svcmpne_s64 (p0, z0, z1),
		p0 = svcmpne (p0, z0, z1))

/*
** cmpne_s64_untied:
**	cmpne	p0\.d, p1/z, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_COMPARE_Z (cmpne_s64_untied, svint64_t,
		p0 = svcmpne_s64 (p1, z0, z1),
		p0 = svcmpne (p1, z0, z1))

/*
** cmpne_x0_s64:
**	mov	(z[0-9]+\.d), x0
**	cmpne	p0\.d, p1/z, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_COMPARE_ZX (cmpne_x0_s64, svint64_t, int64_t,
		 p0 = svcmpne_n_s64 (p1, z0, x0),
		 p0 = svcmpne (p1, z0, x0))

/*
** cmpne_0_s64:
**	cmpne	p0\.d, p1/z, z0\.d, #0
**	ret
*/
TEST_COMPARE_Z (cmpne_0_s64, svint64_t,
		p0 = svcmpne_n_s64 (p1, z0, 0),
		p0 = svcmpne (p1, z0, 0))

/*
** cmpne_1_s64:
**	cmpne	p0\.d, p1/z, z0\.d, #1
**	ret
*/
TEST_COMPARE_Z (cmpne_1_s64, svint64_t,
		p0 = svcmpne_n_s64 (p1, z0, 1),
		p0 = svcmpne (p1, z0, 1))

/*
** cmpne_15_s64:
**	cmpne	p0\.d, p1/z, z0\.d, #15
**	ret
*/
TEST_COMPARE_Z (cmpne_15_s64, svint64_t,
		p0 = svcmpne_n_s64 (p1, z0, 15),
		p0 = svcmpne (p1, z0, 15))

/*
** cmpne_16_s64:
**	mov	(z[0-9]+\.d), #16
**	cmpne	p0\.d, p1/z, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_COMPARE_Z (cmpne_16_s64, svint64_t,
		p0 = svcmpne_n_s64 (p1, z0, 16),
		p0 = svcmpne (p1, z0, 16))

/*
** cmpne_m1_s64:
**	cmpne	p0\.d, p1/z, z0\.d, #-1
**	ret
*/
TEST_COMPARE_Z (cmpne_m1_s64, svint64_t,
		p0 = svcmpne_n_s64 (p1, z0, -1),
		p0 = svcmpne (p1, z0, -1))

/*
** cmpne_m16_s64:
**	cmpne	p0\.d, p1/z, z0\.d, #-16
**	ret
*/
TEST_COMPARE_Z (cmpne_m16_s64, svint64_t,
		p0 = svcmpne_n_s64 (p1, z0, -16),
		p0 = svcmpne (p1, z0, -16))

/*
** cmpne_m17_s64:
**	mov	(z[0-9]+\.d), #-17
**	cmpne	p0\.d, p1/z, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_COMPARE_Z (cmpne_m17_s64, svint64_t,
		p0 = svcmpne_n_s64 (p1, z0, -17),
		p0 = svcmpne (p1, z0, -17))
