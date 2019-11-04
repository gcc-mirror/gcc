/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpge_s16_tied:
** (
**	cmpge	p0\.h, p0/z, z0\.h, z1\.h
** |
**	cmple	p0\.h, p0/z, z1\.h, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_s16_tied, svint16_t,
		p0 = svcmpge_s16 (p0, z0, z1),
		p0 = svcmpge (p0, z0, z1))

/*
** cmpge_s16_untied:
** (
**	cmpge	p0\.h, p1/z, z0\.h, z1\.h
** |
**	cmple	p0\.h, p1/z, z1\.h, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_s16_untied, svint16_t,
		p0 = svcmpge_s16 (p1, z0, z1),
		p0 = svcmpge (p1, z0, z1))

/*
** cmpge_w0_s16:
**	mov	(z[0-9]+\.h), w0
** (
**	cmpge	p0\.h, p1/z, z0\.h, \1
** |
**	cmple	p0\.h, p1/z, \1, z0\.h
** )
**	ret
*/
TEST_COMPARE_ZX (cmpge_w0_s16, svint16_t, int16_t,
		 p0 = svcmpge_n_s16 (p1, z0, x0),
		 p0 = svcmpge (p1, z0, x0))

/*
** cmpge_0_s16:
**	cmpge	p0\.h, p1/z, z0\.h, #0
**	ret
*/
TEST_COMPARE_Z (cmpge_0_s16, svint16_t,
		p0 = svcmpge_n_s16 (p1, z0, 0),
		p0 = svcmpge (p1, z0, 0))

/*
** cmpge_1_s16:
**	cmpge	p0\.h, p1/z, z0\.h, #1
**	ret
*/
TEST_COMPARE_Z (cmpge_1_s16, svint16_t,
		p0 = svcmpge_n_s16 (p1, z0, 1),
		p0 = svcmpge (p1, z0, 1))

/*
** cmpge_15_s16:
**	cmpge	p0\.h, p1/z, z0\.h, #15
**	ret
*/
TEST_COMPARE_Z (cmpge_15_s16, svint16_t,
		p0 = svcmpge_n_s16 (p1, z0, 15),
		p0 = svcmpge (p1, z0, 15))

/*
** cmpge_16_s16:
**	mov	(z[0-9]+\.h), #16
** (
**	cmpge	p0\.h, p1/z, z0\.h, \1
** |
**	cmple	p0\.h, p1/z, \1, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_16_s16, svint16_t,
		p0 = svcmpge_n_s16 (p1, z0, 16),
		p0 = svcmpge (p1, z0, 16))

/*
** cmpge_m1_s16:
**	cmpge	p0\.h, p1/z, z0\.h, #-1
**	ret
*/
TEST_COMPARE_Z (cmpge_m1_s16, svint16_t,
		p0 = svcmpge_n_s16 (p1, z0, -1),
		p0 = svcmpge (p1, z0, -1))

/*
** cmpge_m16_s16:
**	cmpge	p0\.h, p1/z, z0\.h, #-16
**	ret
*/
TEST_COMPARE_Z (cmpge_m16_s16, svint16_t,
		p0 = svcmpge_n_s16 (p1, z0, -16),
		p0 = svcmpge (p1, z0, -16))

/*
** cmpge_m17_s16:
**	mov	(z[0-9]+\.h), #-17
** (
**	cmpge	p0\.h, p1/z, z0\.h, \1
** |
**	cmple	p0\.h, p1/z, \1, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_m17_s16, svint16_t,
		p0 = svcmpge_n_s16 (p1, z0, -17),
		p0 = svcmpge (p1, z0, -17))
