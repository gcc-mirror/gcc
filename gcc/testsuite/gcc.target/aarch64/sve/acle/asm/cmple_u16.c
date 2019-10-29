/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmple_u16_tied:
** (
**	cmphs	p0\.h, p0/z, z1\.h, z0\.h
** |
**	cmpls	p0\.h, p0/z, z0\.h, z1\.h
** )
**	ret
*/
TEST_COMPARE_Z (cmple_u16_tied, svuint16_t,
		p0 = svcmple_u16 (p0, z0, z1),
		p0 = svcmple (p0, z0, z1))

/*
** cmple_u16_untied:
** (
**	cmphs	p0\.h, p1/z, z1\.h, z0\.h
** |
**	cmpls	p0\.h, p1/z, z0\.h, z1\.h
** )
**	ret
*/
TEST_COMPARE_Z (cmple_u16_untied, svuint16_t,
		p0 = svcmple_u16 (p1, z0, z1),
		p0 = svcmple (p1, z0, z1))

/*
** cmple_w0_u16:
**	mov	(z[0-9]+\.h), w0
** (
**	cmphs	p0\.h, p1/z, \1, z0\.h
** |
**	cmpls	p0\.h, p1/z, z0\.h, \1
** )
**	ret
*/
TEST_COMPARE_ZX (cmple_w0_u16, svuint16_t, uint16_t,
		 p0 = svcmple_n_u16 (p1, z0, x0),
		 p0 = svcmple (p1, z0, x0))

/*
** cmple_0_u16:
**	cmpls	p0\.h, p1/z, z0\.h, #0
**	ret
*/
TEST_COMPARE_Z (cmple_0_u16, svuint16_t,
		p0 = svcmple_n_u16 (p1, z0, 0),
		p0 = svcmple (p1, z0, 0))

/*
** cmple_1_u16:
**	cmpls	p0\.h, p1/z, z0\.h, #1
**	ret
*/
TEST_COMPARE_Z (cmple_1_u16, svuint16_t,
		p0 = svcmple_n_u16 (p1, z0, 1),
		p0 = svcmple (p1, z0, 1))

/*
** cmple_15_u16:
**	cmpls	p0\.h, p1/z, z0\.h, #15
**	ret
*/
TEST_COMPARE_Z (cmple_15_u16, svuint16_t,
		p0 = svcmple_n_u16 (p1, z0, 15),
		p0 = svcmple (p1, z0, 15))

/*
** cmple_16_u16:
**	cmpls	p0\.h, p1/z, z0\.h, #16
**	ret
*/
TEST_COMPARE_Z (cmple_16_u16, svuint16_t,
		p0 = svcmple_n_u16 (p1, z0, 16),
		p0 = svcmple (p1, z0, 16))

/*
** cmple_127_u16:
**	cmpls	p0\.h, p1/z, z0\.h, #127
**	ret
*/
TEST_COMPARE_Z (cmple_127_u16, svuint16_t,
		p0 = svcmple_n_u16 (p1, z0, 127),
		p0 = svcmple (p1, z0, 127))

/*
** cmple_128_u16:
**	mov	(z[0-9]+\.h), #128
** (
**	cmphs	p0\.h, p1/z, \1, z0\.h
** |
**	cmpls	p0\.h, p1/z, z0\.h, \1
** )
**	ret
*/
TEST_COMPARE_Z (cmple_128_u16, svuint16_t,
		p0 = svcmple_n_u16 (p1, z0, 128),
		p0 = svcmple (p1, z0, 128))

/*
** cmple_m1_u16:
**	mov	(z[0-9]+)\.b, #-1
** (
**	cmphs	p0\.h, p1/z, \1\.h, z0\.h
** |
**	cmpls	p0\.h, p1/z, z0\.h, \1\.h
** )
**	ret
*/
TEST_COMPARE_Z (cmple_m1_u16, svuint16_t,
		p0 = svcmple_n_u16 (p1, z0, -1),
		p0 = svcmple (p1, z0, -1))
