/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpne_u16_tied:
**	cmpne	p0\.h, p0/z, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_COMPARE_Z (cmpne_u16_tied, svuint16_t,
		p0 = svcmpne_u16 (p0, z0, z1),
		p0 = svcmpne (p0, z0, z1))

/*
** cmpne_u16_untied:
**	cmpne	p0\.h, p1/z, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_COMPARE_Z (cmpne_u16_untied, svuint16_t,
		p0 = svcmpne_u16 (p1, z0, z1),
		p0 = svcmpne (p1, z0, z1))

/*
** cmpne_w0_u16:
**	mov	(z[0-9]+\.h), w0
**	cmpne	p0\.h, p1/z, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_COMPARE_ZX (cmpne_w0_u16, svuint16_t, uint16_t,
		 p0 = svcmpne_n_u16 (p1, z0, x0),
		 p0 = svcmpne (p1, z0, x0))

/*
** cmpne_0_u16:
**	cmpne	p0\.h, p1/z, z0\.h, #0
**	ret
*/
TEST_COMPARE_Z (cmpne_0_u16, svuint16_t,
		p0 = svcmpne_n_u16 (p1, z0, 0),
		p0 = svcmpne (p1, z0, 0))

/*
** cmpne_1_u16:
**	cmpne	p0\.h, p1/z, z0\.h, #1
**	ret
*/
TEST_COMPARE_Z (cmpne_1_u16, svuint16_t,
		p0 = svcmpne_n_u16 (p1, z0, 1),
		p0 = svcmpne (p1, z0, 1))

/*
** cmpne_15_u16:
**	cmpne	p0\.h, p1/z, z0\.h, #15
**	ret
*/
TEST_COMPARE_Z (cmpne_15_u16, svuint16_t,
		p0 = svcmpne_n_u16 (p1, z0, 15),
		p0 = svcmpne (p1, z0, 15))

/*
** cmpne_16_u16:
**	mov	(z[0-9]+\.h), #16
**	cmpne	p0\.h, p1/z, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_COMPARE_Z (cmpne_16_u16, svuint16_t,
		p0 = svcmpne_n_u16 (p1, z0, 16),
		p0 = svcmpne (p1, z0, 16))

/*
** cmpne_m1_u16:
**	cmpne	p0\.h, p1/z, z0\.h, #-1
**	ret
*/
TEST_COMPARE_Z (cmpne_m1_u16, svuint16_t,
		p0 = svcmpne_n_u16 (p1, z0, -1),
		p0 = svcmpne (p1, z0, -1))

/*
** cmpne_m16_u16:
**	cmpne	p0\.h, p1/z, z0\.h, #-16
**	ret
*/
TEST_COMPARE_Z (cmpne_m16_u16, svuint16_t,
		p0 = svcmpne_n_u16 (p1, z0, -16),
		p0 = svcmpne (p1, z0, -16))

/*
** cmpne_m17_u16:
**	mov	(z[0-9]+\.h), #-17
**	cmpne	p0\.h, p1/z, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_COMPARE_Z (cmpne_m17_u16, svuint16_t,
		p0 = svcmpne_n_u16 (p1, z0, -17),
		p0 = svcmpne (p1, z0, -17))
