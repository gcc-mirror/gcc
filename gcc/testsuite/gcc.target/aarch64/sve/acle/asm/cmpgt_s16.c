/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpgt_s16_tied:
** (
**	cmpgt	p0\.h, p0/z, z0\.h, z1\.h
** |
**	cmplt	p0\.h, p0/z, z1\.h, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_s16_tied, svint16_t,
		p0 = svcmpgt_s16 (p0, z0, z1),
		p0 = svcmpgt (p0, z0, z1))

/*
** cmpgt_s16_untied:
** (
**	cmpgt	p0\.h, p1/z, z0\.h, z1\.h
** |
**	cmplt	p0\.h, p1/z, z1\.h, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_s16_untied, svint16_t,
		p0 = svcmpgt_s16 (p1, z0, z1),
		p0 = svcmpgt (p1, z0, z1))

/*
** cmpgt_w0_s16:
**	mov	(z[0-9]+\.h), w0
** (
**	cmpgt	p0\.h, p1/z, z0\.h, \1
** |
**	cmplt	p0\.h, p1/z, \1, z0\.h
** )
**	ret
*/
TEST_COMPARE_ZX (cmpgt_w0_s16, svint16_t, int16_t,
		 p0 = svcmpgt_n_s16 (p1, z0, x0),
		 p0 = svcmpgt (p1, z0, x0))

/*
** cmpgt_0_s16:
**	cmpgt	p0\.h, p1/z, z0\.h, #0
**	ret
*/
TEST_COMPARE_Z (cmpgt_0_s16, svint16_t,
		p0 = svcmpgt_n_s16 (p1, z0, 0),
		p0 = svcmpgt (p1, z0, 0))

/*
** cmpgt_1_s16:
**	cmpgt	p0\.h, p1/z, z0\.h, #1
**	ret
*/
TEST_COMPARE_Z (cmpgt_1_s16, svint16_t,
		p0 = svcmpgt_n_s16 (p1, z0, 1),
		p0 = svcmpgt (p1, z0, 1))

/*
** cmpgt_15_s16:
**	cmpgt	p0\.h, p1/z, z0\.h, #15
**	ret
*/
TEST_COMPARE_Z (cmpgt_15_s16, svint16_t,
		p0 = svcmpgt_n_s16 (p1, z0, 15),
		p0 = svcmpgt (p1, z0, 15))

/*
** cmpgt_16_s16:
**	mov	(z[0-9]+\.h), #16
** (
**	cmpgt	p0\.h, p1/z, z0\.h, \1
** |
**	cmplt	p0\.h, p1/z, \1, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_16_s16, svint16_t,
		p0 = svcmpgt_n_s16 (p1, z0, 16),
		p0 = svcmpgt (p1, z0, 16))

/*
** cmpgt_m1_s16:
**	cmpgt	p0\.h, p1/z, z0\.h, #-1
**	ret
*/
TEST_COMPARE_Z (cmpgt_m1_s16, svint16_t,
		p0 = svcmpgt_n_s16 (p1, z0, -1),
		p0 = svcmpgt (p1, z0, -1))

/*
** cmpgt_m16_s16:
**	cmpgt	p0\.h, p1/z, z0\.h, #-16
**	ret
*/
TEST_COMPARE_Z (cmpgt_m16_s16, svint16_t,
		p0 = svcmpgt_n_s16 (p1, z0, -16),
		p0 = svcmpgt (p1, z0, -16))

/*
** cmpgt_m17_s16:
**	mov	(z[0-9]+\.h), #-17
** (
**	cmpgt	p0\.h, p1/z, z0\.h, \1
** |
**	cmplt	p0\.h, p1/z, \1, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_m17_s16, svint16_t,
		p0 = svcmpgt_n_s16 (p1, z0, -17),
		p0 = svcmpgt (p1, z0, -17))
