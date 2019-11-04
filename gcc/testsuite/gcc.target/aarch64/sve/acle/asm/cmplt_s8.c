/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmplt_s8_tied:
** (
**	cmpgt	p0\.b, p0/z, z1\.b, z0\.b
** |
**	cmplt	p0\.b, p0/z, z0\.b, z1\.b
** )
**	ret
*/
TEST_COMPARE_Z (cmplt_s8_tied, svint8_t,
		p0 = svcmplt_s8 (p0, z0, z1),
		p0 = svcmplt (p0, z0, z1))

/*
** cmplt_s8_untied:
** (
**	cmpgt	p0\.b, p1/z, z1\.b, z0\.b
** |
**	cmplt	p0\.b, p1/z, z0\.b, z1\.b
** )
**	ret
*/
TEST_COMPARE_Z (cmplt_s8_untied, svint8_t,
		p0 = svcmplt_s8 (p1, z0, z1),
		p0 = svcmplt (p1, z0, z1))

/*
** cmplt_w0_s8:
**	mov	(z[0-9]+\.b), w0
** (
**	cmpgt	p0\.b, p1/z, \1, z0\.b
** |
**	cmplt	p0\.b, p1/z, z0\.b, \1
** )
**	ret
*/
TEST_COMPARE_ZX (cmplt_w0_s8, svint8_t, int8_t,
		 p0 = svcmplt_n_s8 (p1, z0, x0),
		 p0 = svcmplt (p1, z0, x0))

/*
** cmplt_0_s8:
**	cmplt	p0\.b, p1/z, z0\.b, #0
**	ret
*/
TEST_COMPARE_Z (cmplt_0_s8, svint8_t,
		p0 = svcmplt_n_s8 (p1, z0, 0),
		p0 = svcmplt (p1, z0, 0))

/*
** cmplt_1_s8:
**	cmplt	p0\.b, p1/z, z0\.b, #1
**	ret
*/
TEST_COMPARE_Z (cmplt_1_s8, svint8_t,
		p0 = svcmplt_n_s8 (p1, z0, 1),
		p0 = svcmplt (p1, z0, 1))

/*
** cmplt_15_s8:
**	cmplt	p0\.b, p1/z, z0\.b, #15
**	ret
*/
TEST_COMPARE_Z (cmplt_15_s8, svint8_t,
		p0 = svcmplt_n_s8 (p1, z0, 15),
		p0 = svcmplt (p1, z0, 15))

/*
** cmplt_16_s8:
**	mov	(z[0-9]+\.b), #16
** (
**	cmpgt	p0\.b, p1/z, \1, z0\.b
** |
**	cmplt	p0\.b, p1/z, z0\.b, \1
** )
**	ret
*/
TEST_COMPARE_Z (cmplt_16_s8, svint8_t,
		p0 = svcmplt_n_s8 (p1, z0, 16),
		p0 = svcmplt (p1, z0, 16))

/*
** cmplt_m1_s8:
**	cmplt	p0\.b, p1/z, z0\.b, #-1
**	ret
*/
TEST_COMPARE_Z (cmplt_m1_s8, svint8_t,
		p0 = svcmplt_n_s8 (p1, z0, -1),
		p0 = svcmplt (p1, z0, -1))

/*
** cmplt_m16_s8:
**	cmplt	p0\.b, p1/z, z0\.b, #-16
**	ret
*/
TEST_COMPARE_Z (cmplt_m16_s8, svint8_t,
		p0 = svcmplt_n_s8 (p1, z0, -16),
		p0 = svcmplt (p1, z0, -16))

/*
** cmplt_m17_s8:
**	mov	(z[0-9]+\.b), #-17
** (
**	cmpgt	p0\.b, p1/z, \1, z0\.b
** |
**	cmplt	p0\.b, p1/z, z0\.b, \1
** )
**	ret
*/
TEST_COMPARE_Z (cmplt_m17_s8, svint8_t,
		p0 = svcmplt_n_s8 (p1, z0, -17),
		p0 = svcmplt (p1, z0, -17))
