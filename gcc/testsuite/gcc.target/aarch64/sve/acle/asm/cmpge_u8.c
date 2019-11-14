/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpge_u8_tied:
** (
**	cmphs	p0\.b, p0/z, z0\.b, z1\.b
** |
**	cmpls	p0\.b, p0/z, z1\.b, z0\.b
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_u8_tied, svuint8_t,
		p0 = svcmpge_u8 (p0, z0, z1),
		p0 = svcmpge (p0, z0, z1))

/*
** cmpge_u8_untied:
** (
**	cmphs	p0\.b, p1/z, z0\.b, z1\.b
** |
**	cmpls	p0\.b, p1/z, z1\.b, z0\.b
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_u8_untied, svuint8_t,
		p0 = svcmpge_u8 (p1, z0, z1),
		p0 = svcmpge (p1, z0, z1))

/*
** cmpge_w0_u8:
**	mov	(z[0-9]+\.b), w0
** (
**	cmphs	p0\.b, p1/z, z0\.b, \1
** |
**	cmpls	p0\.b, p1/z, \1, z0\.b
** )
**	ret
*/
TEST_COMPARE_ZX (cmpge_w0_u8, svuint8_t, uint8_t,
		 p0 = svcmpge_n_u8 (p1, z0, x0),
		 p0 = svcmpge (p1, z0, x0))

/*
** cmpge_0_u8:
**	cmphs	p0\.b, p1/z, z0\.b, #0
**	ret
*/
TEST_COMPARE_Z (cmpge_0_u8, svuint8_t,
		p0 = svcmpge_n_u8 (p1, z0, 0),
		p0 = svcmpge (p1, z0, 0))

/*
** cmpge_1_u8:
**	cmphs	p0\.b, p1/z, z0\.b, #1
**	ret
*/
TEST_COMPARE_Z (cmpge_1_u8, svuint8_t,
		p0 = svcmpge_n_u8 (p1, z0, 1),
		p0 = svcmpge (p1, z0, 1))

/*
** cmpge_15_u8:
**	cmphs	p0\.b, p1/z, z0\.b, #15
**	ret
*/
TEST_COMPARE_Z (cmpge_15_u8, svuint8_t,
		p0 = svcmpge_n_u8 (p1, z0, 15),
		p0 = svcmpge (p1, z0, 15))

/*
** cmpge_16_u8:
**	cmphs	p0\.b, p1/z, z0\.b, #16
**	ret
*/
TEST_COMPARE_Z (cmpge_16_u8, svuint8_t,
		p0 = svcmpge_n_u8 (p1, z0, 16),
		p0 = svcmpge (p1, z0, 16))

/*
** cmpge_127_u8:
**	cmphs	p0\.b, p1/z, z0\.b, #127
**	ret
*/
TEST_COMPARE_Z (cmpge_127_u8, svuint8_t,
		p0 = svcmpge_n_u8 (p1, z0, 127),
		p0 = svcmpge (p1, z0, 127))

/*
** cmpge_128_u8:
**	mov	(z[0-9]+\.b), #-128
** (
**	cmphs	p0\.b, p1/z, z0\.b, \1
** |
**	cmpls	p0\.b, p1/z, \1, z0\.b
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_128_u8, svuint8_t,
		p0 = svcmpge_n_u8 (p1, z0, 128),
		p0 = svcmpge (p1, z0, 128))

/*
** cmpge_m1_u8:
**	mov	(z[0-9]+\.b), #-1
** (
**	cmphs	p0\.b, p1/z, z0\.b, \1
** |
**	cmpls	p0\.b, p1/z, \1, z0\.b
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_m1_u8, svuint8_t,
		p0 = svcmpge_n_u8 (p1, z0, -1),
		p0 = svcmpge (p1, z0, -1))
