/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmplt_u8_tied:
** (
**	cmphi	p0\.b, p0/z, z1\.b, z0\.b
** |
**	cmplo	p0\.b, p0/z, z0\.b, z1\.b
** )
**	ret
*/
TEST_COMPARE_Z (cmplt_u8_tied, svuint8_t,
		p0 = svcmplt_u8 (p0, z0, z1),
		p0 = svcmplt (p0, z0, z1))

/*
** cmplt_u8_untied:
** (
**	cmphi	p0\.b, p1/z, z1\.b, z0\.b
** |
**	cmplo	p0\.b, p1/z, z0\.b, z1\.b
** )
**	ret
*/
TEST_COMPARE_Z (cmplt_u8_untied, svuint8_t,
		p0 = svcmplt_u8 (p1, z0, z1),
		p0 = svcmplt (p1, z0, z1))

/*
** cmplt_w0_u8:
**	mov	(z[0-9]+\.b), w0
** (
**	cmphi	p0\.b, p1/z, \1, z0\.b
** |
**	cmplo	p0\.b, p1/z, z0\.b, \1
** )
**	ret
*/
TEST_COMPARE_ZX (cmplt_w0_u8, svuint8_t, uint8_t,
		 p0 = svcmplt_n_u8 (p1, z0, x0),
		 p0 = svcmplt (p1, z0, x0))

/*
** cmplt_0_u8:
**	cmplo	p0\.b, p1/z, z0\.b, #0
**	ret
*/
TEST_COMPARE_Z (cmplt_0_u8, svuint8_t,
		p0 = svcmplt_n_u8 (p1, z0, 0),
		p0 = svcmplt (p1, z0, 0))

/*
** cmplt_1_u8:
**	cmplo	p0\.b, p1/z, z0\.b, #1
**	ret
*/
TEST_COMPARE_Z (cmplt_1_u8, svuint8_t,
		p0 = svcmplt_n_u8 (p1, z0, 1),
		p0 = svcmplt (p1, z0, 1))

/*
** cmplt_15_u8:
**	cmplo	p0\.b, p1/z, z0\.b, #15
**	ret
*/
TEST_COMPARE_Z (cmplt_15_u8, svuint8_t,
		p0 = svcmplt_n_u8 (p1, z0, 15),
		p0 = svcmplt (p1, z0, 15))

/*
** cmplt_16_u8:
**	cmplo	p0\.b, p1/z, z0\.b, #16
**	ret
*/
TEST_COMPARE_Z (cmplt_16_u8, svuint8_t,
		p0 = svcmplt_n_u8 (p1, z0, 16),
		p0 = svcmplt (p1, z0, 16))

/*
** cmplt_127_u8:
**	cmplo	p0\.b, p1/z, z0\.b, #127
**	ret
*/
TEST_COMPARE_Z (cmplt_127_u8, svuint8_t,
		p0 = svcmplt_n_u8 (p1, z0, 127),
		p0 = svcmplt (p1, z0, 127))

/*
** cmplt_128_u8:
**	mov	(z[0-9]+\.b), #-128
** (
**	cmphi	p0\.b, p1/z, \1, z0\.b
** |
**	cmplo	p0\.b, p1/z, z0\.b, \1
** )
**	ret
*/
TEST_COMPARE_Z (cmplt_128_u8, svuint8_t,
		p0 = svcmplt_n_u8 (p1, z0, 128),
		p0 = svcmplt (p1, z0, 128))

/*
** cmplt_m1_u8:
**	mov	(z[0-9]+\.b), #-1
** (
**	cmphi	p0\.b, p1/z, \1, z0\.b
** |
**	cmplo	p0\.b, p1/z, z0\.b, \1
** )
**	ret
*/
TEST_COMPARE_Z (cmplt_m1_u8, svuint8_t,
		p0 = svcmplt_n_u8 (p1, z0, -1),
		p0 = svcmplt (p1, z0, -1))
