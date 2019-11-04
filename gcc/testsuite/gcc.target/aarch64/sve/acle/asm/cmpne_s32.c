/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpne_s32_tied:
**	cmpne	p0\.s, p0/z, (z0\.s, z1\.s|z1\.s, z0\.s)
**	ret
*/
TEST_COMPARE_Z (cmpne_s32_tied, svint32_t,
		p0 = svcmpne_s32 (p0, z0, z1),
		p0 = svcmpne (p0, z0, z1))

/*
** cmpne_s32_untied:
**	cmpne	p0\.s, p1/z, (z0\.s, z1\.s|z1\.s, z0\.s)
**	ret
*/
TEST_COMPARE_Z (cmpne_s32_untied, svint32_t,
		p0 = svcmpne_s32 (p1, z0, z1),
		p0 = svcmpne (p1, z0, z1))

/*
** cmpne_w0_s32:
**	mov	(z[0-9]+\.s), w0
**	cmpne	p0\.s, p1/z, (z0\.s, \1|\1, z0\.s)
**	ret
*/
TEST_COMPARE_ZX (cmpne_w0_s32, svint32_t, int32_t,
		 p0 = svcmpne_n_s32 (p1, z0, x0),
		 p0 = svcmpne (p1, z0, x0))

/*
** cmpne_0_s32:
**	cmpne	p0\.s, p1/z, z0\.s, #0
**	ret
*/
TEST_COMPARE_Z (cmpne_0_s32, svint32_t,
		p0 = svcmpne_n_s32 (p1, z0, 0),
		p0 = svcmpne (p1, z0, 0))

/*
** cmpne_1_s32:
**	cmpne	p0\.s, p1/z, z0\.s, #1
**	ret
*/
TEST_COMPARE_Z (cmpne_1_s32, svint32_t,
		p0 = svcmpne_n_s32 (p1, z0, 1),
		p0 = svcmpne (p1, z0, 1))

/*
** cmpne_15_s32:
**	cmpne	p0\.s, p1/z, z0\.s, #15
**	ret
*/
TEST_COMPARE_Z (cmpne_15_s32, svint32_t,
		p0 = svcmpne_n_s32 (p1, z0, 15),
		p0 = svcmpne (p1, z0, 15))

/*
** cmpne_16_s32:
**	mov	(z[0-9]+\.s), #16
**	cmpne	p0\.s, p1/z, (z0\.s, \1|\1, z0\.s)
**	ret
*/
TEST_COMPARE_Z (cmpne_16_s32, svint32_t,
		p0 = svcmpne_n_s32 (p1, z0, 16),
		p0 = svcmpne (p1, z0, 16))

/*
** cmpne_m1_s32:
**	cmpne	p0\.s, p1/z, z0\.s, #-1
**	ret
*/
TEST_COMPARE_Z (cmpne_m1_s32, svint32_t,
		p0 = svcmpne_n_s32 (p1, z0, -1),
		p0 = svcmpne (p1, z0, -1))

/*
** cmpne_m16_s32:
**	cmpne	p0\.s, p1/z, z0\.s, #-16
**	ret
*/
TEST_COMPARE_Z (cmpne_m16_s32, svint32_t,
		p0 = svcmpne_n_s32 (p1, z0, -16),
		p0 = svcmpne (p1, z0, -16))

/*
** cmpne_m17_s32:
**	mov	(z[0-9]+\.s), #-17
**	cmpne	p0\.s, p1/z, (z0\.s, \1|\1, z0\.s)
**	ret
*/
TEST_COMPARE_Z (cmpne_m17_s32, svint32_t,
		p0 = svcmpne_n_s32 (p1, z0, -17),
		p0 = svcmpne (p1, z0, -17))
