/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmplt_wide_s16_tied:
**	cmplt	p0\.h, p0/z, z0\.h, z1\.d
**	ret
*/
TEST_COMPARE_DUAL_Z (cmplt_wide_s16_tied, svint16_t, svint64_t,
		     p0 = svcmplt_wide_s16 (p0, z0, z1),
		     p0 = svcmplt_wide (p0, z0, z1))

/*
** cmplt_wide_s16_untied:
**	cmplt	p0\.h, p1/z, z0\.h, z1\.d
**	ret
*/
TEST_COMPARE_DUAL_Z (cmplt_wide_s16_untied, svint16_t, svint64_t,
		     p0 = svcmplt_wide_s16 (p1, z0, z1),
		     p0 = svcmplt_wide (p1, z0, z1))

/*
** cmplt_wide_x0_s16:
**	mov	(z[0-9]+\.d), x0
**	cmplt	p0\.h, p1/z, z0\.h, \1
**	ret
*/
TEST_COMPARE_ZX (cmplt_wide_x0_s16, svint16_t, int64_t,
		 p0 = svcmplt_wide_n_s16 (p1, z0, x0),
		 p0 = svcmplt_wide (p1, z0, x0))

/*
** cmplt_wide_0_s16:
**	cmplt	p0\.h, p1/z, z0\.h, #0
**	ret
*/
TEST_COMPARE_Z (cmplt_wide_0_s16, svint16_t,
		p0 = svcmplt_wide_n_s16 (p1, z0, 0),
		p0 = svcmplt_wide (p1, z0, 0))

/*
** cmplt_wide_1_s16:
**	cmplt	p0\.h, p1/z, z0\.h, #1
**	ret
*/
TEST_COMPARE_Z (cmplt_wide_1_s16, svint16_t,
		p0 = svcmplt_wide_n_s16 (p1, z0, 1),
		p0 = svcmplt_wide (p1, z0, 1))

/*
** cmplt_wide_15_s16:
**	cmplt	p0\.h, p1/z, z0\.h, #15
**	ret
*/
TEST_COMPARE_Z (cmplt_wide_15_s16, svint16_t,
		p0 = svcmplt_wide_n_s16 (p1, z0, 15),
		p0 = svcmplt_wide (p1, z0, 15))

/*
** cmplt_wide_16_s16:
**	mov	(z[0-9]+\.d), #16
**	cmplt	p0\.h, p1/z, z0\.h, \1
**	ret
*/
TEST_COMPARE_Z (cmplt_wide_16_s16, svint16_t,
		p0 = svcmplt_wide_n_s16 (p1, z0, 16),
		p0 = svcmplt_wide (p1, z0, 16))

/*
** cmplt_wide_m1_s16:
**	cmplt	p0\.h, p1/z, z0\.h, #-1
**	ret
*/
TEST_COMPARE_Z (cmplt_wide_m1_s16, svint16_t,
		p0 = svcmplt_wide_n_s16 (p1, z0, -1),
		p0 = svcmplt_wide (p1, z0, -1))

/*
** cmplt_wide_m16_s16:
**	cmplt	p0\.h, p1/z, z0\.h, #-16
**	ret
*/
TEST_COMPARE_Z (cmplt_wide_m16_s16, svint16_t,
		p0 = svcmplt_wide_n_s16 (p1, z0, -16),
		p0 = svcmplt_wide (p1, z0, -16))

/*
** cmplt_wide_m17_s16:
**	mov	(z[0-9]+\.d), #-17
**	cmplt	p0\.h, p1/z, z0\.h, \1
**	ret
*/
TEST_COMPARE_Z (cmplt_wide_m17_s16, svint16_t,
		p0 = svcmplt_wide_n_s16 (p1, z0, -17),
		p0 = svcmplt_wide (p1, z0, -17))
