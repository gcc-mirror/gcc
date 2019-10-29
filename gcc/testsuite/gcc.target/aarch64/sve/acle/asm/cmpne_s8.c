/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpne_s8_tied:
**	cmpne	p0\.b, p0/z, (z0\.b, z1\.b|z1\.b, z0\.b)
**	ret
*/
TEST_COMPARE_Z (cmpne_s8_tied, svint8_t,
		p0 = svcmpne_s8 (p0, z0, z1),
		p0 = svcmpne (p0, z0, z1))

/*
** cmpne_s8_untied:
**	cmpne	p0\.b, p1/z, (z0\.b, z1\.b|z1\.b, z0\.b)
**	ret
*/
TEST_COMPARE_Z (cmpne_s8_untied, svint8_t,
		p0 = svcmpne_s8 (p1, z0, z1),
		p0 = svcmpne (p1, z0, z1))

/*
** cmpne_w0_s8:
**	mov	(z[0-9]+\.b), w0
**	cmpne	p0\.b, p1/z, (z0\.b, \1|\1, z0\.b)
**	ret
*/
TEST_COMPARE_ZX (cmpne_w0_s8, svint8_t, int8_t,
		 p0 = svcmpne_n_s8 (p1, z0, x0),
		 p0 = svcmpne (p1, z0, x0))

/*
** cmpne_0_s8:
**	cmpne	p0\.b, p1/z, z0\.b, #0
**	ret
*/
TEST_COMPARE_Z (cmpne_0_s8, svint8_t,
		p0 = svcmpne_n_s8 (p1, z0, 0),
		p0 = svcmpne (p1, z0, 0))

/*
** cmpne_1_s8:
**	cmpne	p0\.b, p1/z, z0\.b, #1
**	ret
*/
TEST_COMPARE_Z (cmpne_1_s8, svint8_t,
		p0 = svcmpne_n_s8 (p1, z0, 1),
		p0 = svcmpne (p1, z0, 1))

/*
** cmpne_15_s8:
**	cmpne	p0\.b, p1/z, z0\.b, #15
**	ret
*/
TEST_COMPARE_Z (cmpne_15_s8, svint8_t,
		p0 = svcmpne_n_s8 (p1, z0, 15),
		p0 = svcmpne (p1, z0, 15))

/*
** cmpne_16_s8:
**	mov	(z[0-9]+\.b), #16
**	cmpne	p0\.b, p1/z, (z0\.b, \1|\1, z0\.b)
**	ret
*/
TEST_COMPARE_Z (cmpne_16_s8, svint8_t,
		p0 = svcmpne_n_s8 (p1, z0, 16),
		p0 = svcmpne (p1, z0, 16))

/*
** cmpne_m1_s8:
**	cmpne	p0\.b, p1/z, z0\.b, #-1
**	ret
*/
TEST_COMPARE_Z (cmpne_m1_s8, svint8_t,
		p0 = svcmpne_n_s8 (p1, z0, -1),
		p0 = svcmpne (p1, z0, -1))

/*
** cmpne_m16_s8:
**	cmpne	p0\.b, p1/z, z0\.b, #-16
**	ret
*/
TEST_COMPARE_Z (cmpne_m16_s8, svint8_t,
		p0 = svcmpne_n_s8 (p1, z0, -16),
		p0 = svcmpne (p1, z0, -16))

/*
** cmpne_m17_s8:
**	mov	(z[0-9]+\.b), #-17
**	cmpne	p0\.b, p1/z, (z0\.b, \1|\1, z0\.b)
**	ret
*/
TEST_COMPARE_Z (cmpne_m17_s8, svint8_t,
		p0 = svcmpne_n_s8 (p1, z0, -17),
		p0 = svcmpne (p1, z0, -17))
