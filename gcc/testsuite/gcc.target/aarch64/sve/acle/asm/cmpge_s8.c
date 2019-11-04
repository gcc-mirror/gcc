/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpge_s8_tied:
** (
**	cmpge	p0\.b, p0/z, z0\.b, z1\.b
** |
**	cmple	p0\.b, p0/z, z1\.b, z0\.b
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_s8_tied, svint8_t,
		p0 = svcmpge_s8 (p0, z0, z1),
		p0 = svcmpge (p0, z0, z1))

/*
** cmpge_s8_untied:
** (
**	cmpge	p0\.b, p1/z, z0\.b, z1\.b
** |
**	cmple	p0\.b, p1/z, z1\.b, z0\.b
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_s8_untied, svint8_t,
		p0 = svcmpge_s8 (p1, z0, z1),
		p0 = svcmpge (p1, z0, z1))

/*
** cmpge_w0_s8:
**	mov	(z[0-9]+\.b), w0
** (
**	cmpge	p0\.b, p1/z, z0\.b, \1
** |
**	cmple	p0\.b, p1/z, \1, z0\.b
** )
**	ret
*/
TEST_COMPARE_ZX (cmpge_w0_s8, svint8_t, int8_t,
		 p0 = svcmpge_n_s8 (p1, z0, x0),
		 p0 = svcmpge (p1, z0, x0))

/*
** cmpge_0_s8:
**	cmpge	p0\.b, p1/z, z0\.b, #0
**	ret
*/
TEST_COMPARE_Z (cmpge_0_s8, svint8_t,
		p0 = svcmpge_n_s8 (p1, z0, 0),
		p0 = svcmpge (p1, z0, 0))

/*
** cmpge_1_s8:
**	cmpge	p0\.b, p1/z, z0\.b, #1
**	ret
*/
TEST_COMPARE_Z (cmpge_1_s8, svint8_t,
		p0 = svcmpge_n_s8 (p1, z0, 1),
		p0 = svcmpge (p1, z0, 1))

/*
** cmpge_15_s8:
**	cmpge	p0\.b, p1/z, z0\.b, #15
**	ret
*/
TEST_COMPARE_Z (cmpge_15_s8, svint8_t,
		p0 = svcmpge_n_s8 (p1, z0, 15),
		p0 = svcmpge (p1, z0, 15))

/*
** cmpge_16_s8:
**	mov	(z[0-9]+\.b), #16
** (
**	cmpge	p0\.b, p1/z, z0\.b, \1
** |
**	cmple	p0\.b, p1/z, \1, z0\.b
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_16_s8, svint8_t,
		p0 = svcmpge_n_s8 (p1, z0, 16),
		p0 = svcmpge (p1, z0, 16))

/*
** cmpge_m1_s8:
**	cmpge	p0\.b, p1/z, z0\.b, #-1
**	ret
*/
TEST_COMPARE_Z (cmpge_m1_s8, svint8_t,
		p0 = svcmpge_n_s8 (p1, z0, -1),
		p0 = svcmpge (p1, z0, -1))

/*
** cmpge_m16_s8:
**	cmpge	p0\.b, p1/z, z0\.b, #-16
**	ret
*/
TEST_COMPARE_Z (cmpge_m16_s8, svint8_t,
		p0 = svcmpge_n_s8 (p1, z0, -16),
		p0 = svcmpge (p1, z0, -16))

/*
** cmpge_m17_s8:
**	mov	(z[0-9]+\.b), #-17
** (
**	cmpge	p0\.b, p1/z, z0\.b, \1
** |
**	cmple	p0\.b, p1/z, \1, z0\.b
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_m17_s8, svint8_t,
		p0 = svcmpge_n_s8 (p1, z0, -17),
		p0 = svcmpge (p1, z0, -17))
