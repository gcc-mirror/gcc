/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmplt_s32_tied:
** (
**	cmpgt	p0\.s, p0/z, z1\.s, z0\.s
** |
**	cmplt	p0\.s, p0/z, z0\.s, z1\.s
** )
**	ret
*/
TEST_COMPARE_Z (cmplt_s32_tied, svint32_t,
		p0 = svcmplt_s32 (p0, z0, z1),
		p0 = svcmplt (p0, z0, z1))

/*
** cmplt_s32_untied:
** (
**	cmpgt	p0\.s, p1/z, z1\.s, z0\.s
** |
**	cmplt	p0\.s, p1/z, z0\.s, z1\.s
** )
**	ret
*/
TEST_COMPARE_Z (cmplt_s32_untied, svint32_t,
		p0 = svcmplt_s32 (p1, z0, z1),
		p0 = svcmplt (p1, z0, z1))

/*
** cmplt_w0_s32:
**	mov	(z[0-9]+\.s), w0
** (
**	cmpgt	p0\.s, p1/z, \1, z0\.s
** |
**	cmplt	p0\.s, p1/z, z0\.s, \1
** )
**	ret
*/
TEST_COMPARE_ZX (cmplt_w0_s32, svint32_t, int32_t,
		 p0 = svcmplt_n_s32 (p1, z0, x0),
		 p0 = svcmplt (p1, z0, x0))

/*
** cmplt_0_s32:
**	cmplt	p0\.s, p1/z, z0\.s, #0
**	ret
*/
TEST_COMPARE_Z (cmplt_0_s32, svint32_t,
		p0 = svcmplt_n_s32 (p1, z0, 0),
		p0 = svcmplt (p1, z0, 0))

/*
** cmplt_1_s32:
**	cmplt	p0\.s, p1/z, z0\.s, #1
**	ret
*/
TEST_COMPARE_Z (cmplt_1_s32, svint32_t,
		p0 = svcmplt_n_s32 (p1, z0, 1),
		p0 = svcmplt (p1, z0, 1))

/*
** cmplt_15_s32:
**	cmplt	p0\.s, p1/z, z0\.s, #15
**	ret
*/
TEST_COMPARE_Z (cmplt_15_s32, svint32_t,
		p0 = svcmplt_n_s32 (p1, z0, 15),
		p0 = svcmplt (p1, z0, 15))

/*
** cmplt_16_s32:
**	mov	(z[0-9]+\.s), #16
** (
**	cmpgt	p0\.s, p1/z, \1, z0\.s
** |
**	cmplt	p0\.s, p1/z, z0\.s, \1
** )
**	ret
*/
TEST_COMPARE_Z (cmplt_16_s32, svint32_t,
		p0 = svcmplt_n_s32 (p1, z0, 16),
		p0 = svcmplt (p1, z0, 16))

/*
** cmplt_m1_s32:
**	cmplt	p0\.s, p1/z, z0\.s, #-1
**	ret
*/
TEST_COMPARE_Z (cmplt_m1_s32, svint32_t,
		p0 = svcmplt_n_s32 (p1, z0, -1),
		p0 = svcmplt (p1, z0, -1))

/*
** cmplt_m16_s32:
**	cmplt	p0\.s, p1/z, z0\.s, #-16
**	ret
*/
TEST_COMPARE_Z (cmplt_m16_s32, svint32_t,
		p0 = svcmplt_n_s32 (p1, z0, -16),
		p0 = svcmplt (p1, z0, -16))

/*
** cmplt_m17_s32:
**	mov	(z[0-9]+\.s), #-17
** (
**	cmpgt	p0\.s, p1/z, \1, z0\.s
** |
**	cmplt	p0\.s, p1/z, z0\.s, \1
** )
**	ret
*/
TEST_COMPARE_Z (cmplt_m17_s32, svint32_t,
		p0 = svcmplt_n_s32 (p1, z0, -17),
		p0 = svcmplt (p1, z0, -17))
