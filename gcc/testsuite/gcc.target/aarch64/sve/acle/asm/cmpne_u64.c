/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpne_u64_tied:
**	cmpne	p0\.d, p0/z, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_COMPARE_Z (cmpne_u64_tied, svuint64_t,
		p0 = svcmpne_u64 (p0, z0, z1),
		p0 = svcmpne (p0, z0, z1))

/*
** cmpne_u64_untied:
**	cmpne	p0\.d, p1/z, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_COMPARE_Z (cmpne_u64_untied, svuint64_t,
		p0 = svcmpne_u64 (p1, z0, z1),
		p0 = svcmpne (p1, z0, z1))

/*
** cmpne_x0_u64:
**	mov	(z[0-9]+\.d), x0
**	cmpne	p0\.d, p1/z, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_COMPARE_ZX (cmpne_x0_u64, svuint64_t, uint64_t,
		 p0 = svcmpne_n_u64 (p1, z0, x0),
		 p0 = svcmpne (p1, z0, x0))

/*
** cmpne_0_u64:
**	cmpne	p0\.d, p1/z, z0\.d, #0
**	ret
*/
TEST_COMPARE_Z (cmpne_0_u64, svuint64_t,
		p0 = svcmpne_n_u64 (p1, z0, 0),
		p0 = svcmpne (p1, z0, 0))

/*
** cmpne_1_u64:
**	cmpne	p0\.d, p1/z, z0\.d, #1
**	ret
*/
TEST_COMPARE_Z (cmpne_1_u64, svuint64_t,
		p0 = svcmpne_n_u64 (p1, z0, 1),
		p0 = svcmpne (p1, z0, 1))

/*
** cmpne_15_u64:
**	cmpne	p0\.d, p1/z, z0\.d, #15
**	ret
*/
TEST_COMPARE_Z (cmpne_15_u64, svuint64_t,
		p0 = svcmpne_n_u64 (p1, z0, 15),
		p0 = svcmpne (p1, z0, 15))

/*
** cmpne_16_u64:
**	mov	(z[0-9]+\.d), #16
**	cmpne	p0\.d, p1/z, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_COMPARE_Z (cmpne_16_u64, svuint64_t,
		p0 = svcmpne_n_u64 (p1, z0, 16),
		p0 = svcmpne (p1, z0, 16))

/*
** cmpne_m1_u64:
**	cmpne	p0\.d, p1/z, z0\.d, #-1
**	ret
*/
TEST_COMPARE_Z (cmpne_m1_u64, svuint64_t,
		p0 = svcmpne_n_u64 (p1, z0, -1),
		p0 = svcmpne (p1, z0, -1))

/*
** cmpne_m16_u64:
**	cmpne	p0\.d, p1/z, z0\.d, #-16
**	ret
*/
TEST_COMPARE_Z (cmpne_m16_u64, svuint64_t,
		p0 = svcmpne_n_u64 (p1, z0, -16),
		p0 = svcmpne (p1, z0, -16))

/*
** cmpne_m17_u64:
**	mov	(z[0-9]+\.d), #-17
**	cmpne	p0\.d, p1/z, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_COMPARE_Z (cmpne_m17_u64, svuint64_t,
		p0 = svcmpne_n_u64 (p1, z0, -17),
		p0 = svcmpne (p1, z0, -17))
