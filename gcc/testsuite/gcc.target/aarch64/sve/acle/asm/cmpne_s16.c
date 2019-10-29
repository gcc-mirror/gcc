/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpne_s16_tied:
**	cmpne	p0\.h, p0/z, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_COMPARE_Z (cmpne_s16_tied, svint16_t,
		p0 = svcmpne_s16 (p0, z0, z1),
		p0 = svcmpne (p0, z0, z1))

/*
** cmpne_s16_untied:
**	cmpne	p0\.h, p1/z, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_COMPARE_Z (cmpne_s16_untied, svint16_t,
		p0 = svcmpne_s16 (p1, z0, z1),
		p0 = svcmpne (p1, z0, z1))

/*
** cmpne_w0_s16:
**	mov	(z[0-9]+\.h), w0
**	cmpne	p0\.h, p1/z, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_COMPARE_ZX (cmpne_w0_s16, svint16_t, int16_t,
		 p0 = svcmpne_n_s16 (p1, z0, x0),
		 p0 = svcmpne (p1, z0, x0))

/*
** cmpne_0_s16:
**	cmpne	p0\.h, p1/z, z0\.h, #0
**	ret
*/
TEST_COMPARE_Z (cmpne_0_s16, svint16_t,
		p0 = svcmpne_n_s16 (p1, z0, 0),
		p0 = svcmpne (p1, z0, 0))

/*
** cmpne_1_s16:
**	cmpne	p0\.h, p1/z, z0\.h, #1
**	ret
*/
TEST_COMPARE_Z (cmpne_1_s16, svint16_t,
		p0 = svcmpne_n_s16 (p1, z0, 1),
		p0 = svcmpne (p1, z0, 1))

/*
** cmpne_15_s16:
**	cmpne	p0\.h, p1/z, z0\.h, #15
**	ret
*/
TEST_COMPARE_Z (cmpne_15_s16, svint16_t,
		p0 = svcmpne_n_s16 (p1, z0, 15),
		p0 = svcmpne (p1, z0, 15))

/*
** cmpne_16_s16:
**	mov	(z[0-9]+\.h), #16
**	cmpne	p0\.h, p1/z, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_COMPARE_Z (cmpne_16_s16, svint16_t,
		p0 = svcmpne_n_s16 (p1, z0, 16),
		p0 = svcmpne (p1, z0, 16))

/*
** cmpne_m1_s16:
**	cmpne	p0\.h, p1/z, z0\.h, #-1
**	ret
*/
TEST_COMPARE_Z (cmpne_m1_s16, svint16_t,
		p0 = svcmpne_n_s16 (p1, z0, -1),
		p0 = svcmpne (p1, z0, -1))

/*
** cmpne_m16_s16:
**	cmpne	p0\.h, p1/z, z0\.h, #-16
**	ret
*/
TEST_COMPARE_Z (cmpne_m16_s16, svint16_t,
		p0 = svcmpne_n_s16 (p1, z0, -16),
		p0 = svcmpne (p1, z0, -16))

/*
** cmpne_m17_s16:
**	mov	(z[0-9]+\.h), #-17
**	cmpne	p0\.h, p1/z, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_COMPARE_Z (cmpne_m17_s16, svint16_t,
		p0 = svcmpne_n_s16 (p1, z0, -17),
		p0 = svcmpne (p1, z0, -17))
