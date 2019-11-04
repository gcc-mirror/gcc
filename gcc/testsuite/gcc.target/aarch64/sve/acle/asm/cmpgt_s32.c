/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpgt_s32_tied:
** (
**	cmpgt	p0\.s, p0/z, z0\.s, z1\.s
** |
**	cmplt	p0\.s, p0/z, z1\.s, z0\.s
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_s32_tied, svint32_t,
		p0 = svcmpgt_s32 (p0, z0, z1),
		p0 = svcmpgt (p0, z0, z1))

/*
** cmpgt_s32_untied:
** (
**	cmpgt	p0\.s, p1/z, z0\.s, z1\.s
** |
**	cmplt	p0\.s, p1/z, z1\.s, z0\.s
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_s32_untied, svint32_t,
		p0 = svcmpgt_s32 (p1, z0, z1),
		p0 = svcmpgt (p1, z0, z1))

/*
** cmpgt_w0_s32:
**	mov	(z[0-9]+\.s), w0
** (
**	cmpgt	p0\.s, p1/z, z0\.s, \1
** |
**	cmplt	p0\.s, p1/z, \1, z0\.s
** )
**	ret
*/
TEST_COMPARE_ZX (cmpgt_w0_s32, svint32_t, int32_t,
		 p0 = svcmpgt_n_s32 (p1, z0, x0),
		 p0 = svcmpgt (p1, z0, x0))

/*
** cmpgt_0_s32:
**	cmpgt	p0\.s, p1/z, z0\.s, #0
**	ret
*/
TEST_COMPARE_Z (cmpgt_0_s32, svint32_t,
		p0 = svcmpgt_n_s32 (p1, z0, 0),
		p0 = svcmpgt (p1, z0, 0))

/*
** cmpgt_1_s32:
**	cmpgt	p0\.s, p1/z, z0\.s, #1
**	ret
*/
TEST_COMPARE_Z (cmpgt_1_s32, svint32_t,
		p0 = svcmpgt_n_s32 (p1, z0, 1),
		p0 = svcmpgt (p1, z0, 1))

/*
** cmpgt_15_s32:
**	cmpgt	p0\.s, p1/z, z0\.s, #15
**	ret
*/
TEST_COMPARE_Z (cmpgt_15_s32, svint32_t,
		p0 = svcmpgt_n_s32 (p1, z0, 15),
		p0 = svcmpgt (p1, z0, 15))

/*
** cmpgt_16_s32:
**	mov	(z[0-9]+\.s), #16
** (
**	cmpgt	p0\.s, p1/z, z0\.s, \1
** |
**	cmplt	p0\.s, p1/z, \1, z0\.s
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_16_s32, svint32_t,
		p0 = svcmpgt_n_s32 (p1, z0, 16),
		p0 = svcmpgt (p1, z0, 16))

/*
** cmpgt_m1_s32:
**	cmpgt	p0\.s, p1/z, z0\.s, #-1
**	ret
*/
TEST_COMPARE_Z (cmpgt_m1_s32, svint32_t,
		p0 = svcmpgt_n_s32 (p1, z0, -1),
		p0 = svcmpgt (p1, z0, -1))

/*
** cmpgt_m16_s32:
**	cmpgt	p0\.s, p1/z, z0\.s, #-16
**	ret
*/
TEST_COMPARE_Z (cmpgt_m16_s32, svint32_t,
		p0 = svcmpgt_n_s32 (p1, z0, -16),
		p0 = svcmpgt (p1, z0, -16))

/*
** cmpgt_m17_s32:
**	mov	(z[0-9]+\.s), #-17
** (
**	cmpgt	p0\.s, p1/z, z0\.s, \1
** |
**	cmplt	p0\.s, p1/z, \1, z0\.s
** )
**	ret
*/
TEST_COMPARE_Z (cmpgt_m17_s32, svint32_t,
		p0 = svcmpgt_n_s32 (p1, z0, -17),
		p0 = svcmpgt (p1, z0, -17))
