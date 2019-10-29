/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpge_u16_tied:
** (
**	cmphs	p0\.h, p0/z, z0\.h, z1\.h
** |
**	cmpls	p0\.h, p0/z, z1\.h, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_u16_tied, svuint16_t,
		p0 = svcmpge_u16 (p0, z0, z1),
		p0 = svcmpge (p0, z0, z1))

/*
** cmpge_u16_untied:
** (
**	cmphs	p0\.h, p1/z, z0\.h, z1\.h
** |
**	cmpls	p0\.h, p1/z, z1\.h, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_u16_untied, svuint16_t,
		p0 = svcmpge_u16 (p1, z0, z1),
		p0 = svcmpge (p1, z0, z1))

/*
** cmpge_w0_u16:
**	mov	(z[0-9]+\.h), w0
** (
**	cmphs	p0\.h, p1/z, z0\.h, \1
** |
**	cmpls	p0\.h, p1/z, \1, z0\.h
** )
**	ret
*/
TEST_COMPARE_ZX (cmpge_w0_u16, svuint16_t, uint16_t,
		 p0 = svcmpge_n_u16 (p1, z0, x0),
		 p0 = svcmpge (p1, z0, x0))

/*
** cmpge_0_u16:
**	cmphs	p0\.h, p1/z, z0\.h, #0
**	ret
*/
TEST_COMPARE_Z (cmpge_0_u16, svuint16_t,
		p0 = svcmpge_n_u16 (p1, z0, 0),
		p0 = svcmpge (p1, z0, 0))

/*
** cmpge_1_u16:
**	cmphs	p0\.h, p1/z, z0\.h, #1
**	ret
*/
TEST_COMPARE_Z (cmpge_1_u16, svuint16_t,
		p0 = svcmpge_n_u16 (p1, z0, 1),
		p0 = svcmpge (p1, z0, 1))

/*
** cmpge_15_u16:
**	cmphs	p0\.h, p1/z, z0\.h, #15
**	ret
*/
TEST_COMPARE_Z (cmpge_15_u16, svuint16_t,
		p0 = svcmpge_n_u16 (p1, z0, 15),
		p0 = svcmpge (p1, z0, 15))

/*
** cmpge_16_u16:
**	cmphs	p0\.h, p1/z, z0\.h, #16
**	ret
*/
TEST_COMPARE_Z (cmpge_16_u16, svuint16_t,
		p0 = svcmpge_n_u16 (p1, z0, 16),
		p0 = svcmpge (p1, z0, 16))

/*
** cmpge_127_u16:
**	cmphs	p0\.h, p1/z, z0\.h, #127
**	ret
*/
TEST_COMPARE_Z (cmpge_127_u16, svuint16_t,
		p0 = svcmpge_n_u16 (p1, z0, 127),
		p0 = svcmpge (p1, z0, 127))

/*
** cmpge_128_u16:
**	mov	(z[0-9]+\.h), #128
** (
**	cmphs	p0\.h, p1/z, z0\.h, \1
** |
**	cmpls	p0\.h, p1/z, \1, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_128_u16, svuint16_t,
		p0 = svcmpge_n_u16 (p1, z0, 128),
		p0 = svcmpge (p1, z0, 128))

/*
** cmpge_m1_u16:
**	mov	(z[0-9]+)\.b, #-1
** (
**	cmphs	p0\.h, p1/z, z0\.h, \1\.h
** |
**	cmpls	p0\.h, p1/z, \1\.h, z0\.h
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_m1_u16, svuint16_t,
		p0 = svcmpge_n_u16 (p1, z0, -1),
		p0 = svcmpge (p1, z0, -1))
