/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmplt_wide_s8_tied:
**	cmplt	p0\.b, p0/z, z0\.b, z1\.d
**	ret
*/
TEST_COMPARE_DUAL_Z (cmplt_wide_s8_tied, svint8_t, svint64_t,
		     p0 = svcmplt_wide_s8 (p0, z0, z1),
		     p0 = svcmplt_wide (p0, z0, z1))

/*
** cmplt_wide_s8_untied:
**	cmplt	p0\.b, p1/z, z0\.b, z1\.d
**	ret
*/
TEST_COMPARE_DUAL_Z (cmplt_wide_s8_untied, svint8_t, svint64_t,
		     p0 = svcmplt_wide_s8 (p1, z0, z1),
		     p0 = svcmplt_wide (p1, z0, z1))

/*
** cmplt_wide_x0_s8:
**	mov	(z[0-9]+\.d), x0
**	cmplt	p0\.b, p1/z, z0\.b, \1
**	ret
*/
TEST_COMPARE_ZX (cmplt_wide_x0_s8, svint8_t, int64_t,
		 p0 = svcmplt_wide_n_s8 (p1, z0, x0),
		 p0 = svcmplt_wide (p1, z0, x0))

/*
** cmplt_wide_0_s8:
**	cmplt	p0\.b, p1/z, z0\.b, #0
**	ret
*/
TEST_COMPARE_Z (cmplt_wide_0_s8, svint8_t,
		p0 = svcmplt_wide_n_s8 (p1, z0, 0),
		p0 = svcmplt_wide (p1, z0, 0))

/*
** cmplt_wide_1_s8:
**	cmplt	p0\.b, p1/z, z0\.b, #1
**	ret
*/
TEST_COMPARE_Z (cmplt_wide_1_s8, svint8_t,
		p0 = svcmplt_wide_n_s8 (p1, z0, 1),
		p0 = svcmplt_wide (p1, z0, 1))

/*
** cmplt_wide_15_s8:
**	cmplt	p0\.b, p1/z, z0\.b, #15
**	ret
*/
TEST_COMPARE_Z (cmplt_wide_15_s8, svint8_t,
		p0 = svcmplt_wide_n_s8 (p1, z0, 15),
		p0 = svcmplt_wide (p1, z0, 15))

/*
** cmplt_wide_16_s8:
**	mov	(z[0-9]+\.d), #16
**	cmplt	p0\.b, p1/z, z0\.b, \1
**	ret
*/
TEST_COMPARE_Z (cmplt_wide_16_s8, svint8_t,
		p0 = svcmplt_wide_n_s8 (p1, z0, 16),
		p0 = svcmplt_wide (p1, z0, 16))

/*
** cmplt_wide_m1_s8:
**	cmplt	p0\.b, p1/z, z0\.b, #-1
**	ret
*/
TEST_COMPARE_Z (cmplt_wide_m1_s8, svint8_t,
		p0 = svcmplt_wide_n_s8 (p1, z0, -1),
		p0 = svcmplt_wide (p1, z0, -1))

/*
** cmplt_wide_m16_s8:
**	cmplt	p0\.b, p1/z, z0\.b, #-16
**	ret
*/
TEST_COMPARE_Z (cmplt_wide_m16_s8, svint8_t,
		p0 = svcmplt_wide_n_s8 (p1, z0, -16),
		p0 = svcmplt_wide (p1, z0, -16))

/*
** cmplt_wide_m17_s8:
**	mov	(z[0-9]+\.d), #-17
**	cmplt	p0\.b, p1/z, z0\.b, \1
**	ret
*/
TEST_COMPARE_Z (cmplt_wide_m17_s8, svint8_t,
		p0 = svcmplt_wide_n_s8 (p1, z0, -17),
		p0 = svcmplt_wide (p1, z0, -17))
