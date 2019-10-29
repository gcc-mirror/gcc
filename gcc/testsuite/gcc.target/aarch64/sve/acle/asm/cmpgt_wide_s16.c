/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpgt_wide_s16_tied:
**	cmpgt	p0\.h, p0/z, z0\.h, z1\.d
**	ret
*/
TEST_COMPARE_DUAL_Z (cmpgt_wide_s16_tied, svint16_t, svint64_t,
		     p0 = svcmpgt_wide_s16 (p0, z0, z1),
		     p0 = svcmpgt_wide (p0, z0, z1))

/*
** cmpgt_wide_s16_untied:
**	cmpgt	p0\.h, p1/z, z0\.h, z1\.d
**	ret
*/
TEST_COMPARE_DUAL_Z (cmpgt_wide_s16_untied, svint16_t, svint64_t,
		     p0 = svcmpgt_wide_s16 (p1, z0, z1),
		     p0 = svcmpgt_wide (p1, z0, z1))

/*
** cmpgt_wide_x0_s16:
**	mov	(z[0-9]+\.d), x0
**	cmpgt	p0\.h, p1/z, z0\.h, \1
**	ret
*/
TEST_COMPARE_ZX (cmpgt_wide_x0_s16, svint16_t, int64_t,
		 p0 = svcmpgt_wide_n_s16 (p1, z0, x0),
		 p0 = svcmpgt_wide (p1, z0, x0))

/*
** cmpgt_wide_0_s16:
**	cmpgt	p0\.h, p1/z, z0\.h, #0
**	ret
*/
TEST_COMPARE_Z (cmpgt_wide_0_s16, svint16_t,
		p0 = svcmpgt_wide_n_s16 (p1, z0, 0),
		p0 = svcmpgt_wide (p1, z0, 0))

/*
** cmpgt_wide_1_s16:
**	cmpgt	p0\.h, p1/z, z0\.h, #1
**	ret
*/
TEST_COMPARE_Z (cmpgt_wide_1_s16, svint16_t,
		p0 = svcmpgt_wide_n_s16 (p1, z0, 1),
		p0 = svcmpgt_wide (p1, z0, 1))

/*
** cmpgt_wide_15_s16:
**	cmpgt	p0\.h, p1/z, z0\.h, #15
**	ret
*/
TEST_COMPARE_Z (cmpgt_wide_15_s16, svint16_t,
		p0 = svcmpgt_wide_n_s16 (p1, z0, 15),
		p0 = svcmpgt_wide (p1, z0, 15))

/*
** cmpgt_wide_16_s16:
**	mov	(z[0-9]+\.d), #16
**	cmpgt	p0\.h, p1/z, z0\.h, \1
**	ret
*/
TEST_COMPARE_Z (cmpgt_wide_16_s16, svint16_t,
		p0 = svcmpgt_wide_n_s16 (p1, z0, 16),
		p0 = svcmpgt_wide (p1, z0, 16))

/*
** cmpgt_wide_m1_s16:
**	cmpgt	p0\.h, p1/z, z0\.h, #-1
**	ret
*/
TEST_COMPARE_Z (cmpgt_wide_m1_s16, svint16_t,
		p0 = svcmpgt_wide_n_s16 (p1, z0, -1),
		p0 = svcmpgt_wide (p1, z0, -1))

/*
** cmpgt_wide_m16_s16:
**	cmpgt	p0\.h, p1/z, z0\.h, #-16
**	ret
*/
TEST_COMPARE_Z (cmpgt_wide_m16_s16, svint16_t,
		p0 = svcmpgt_wide_n_s16 (p1, z0, -16),
		p0 = svcmpgt_wide (p1, z0, -16))

/*
** cmpgt_wide_m17_s16:
**	mov	(z[0-9]+\.d), #-17
**	cmpgt	p0\.h, p1/z, z0\.h, \1
**	ret
*/
TEST_COMPARE_Z (cmpgt_wide_m17_s16, svint16_t,
		p0 = svcmpgt_wide_n_s16 (p1, z0, -17),
		p0 = svcmpgt_wide (p1, z0, -17))
