/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpge_s32_tied:
** (
**	cmpge	p0\.s, p0/z, z0\.s, z1\.s
** |
**	cmple	p0\.s, p0/z, z1\.s, z0\.s
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_s32_tied, svint32_t,
		p0 = svcmpge_s32 (p0, z0, z1),
		p0 = svcmpge (p0, z0, z1))

/*
** cmpge_s32_untied:
** (
**	cmpge	p0\.s, p1/z, z0\.s, z1\.s
** |
**	cmple	p0\.s, p1/z, z1\.s, z0\.s
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_s32_untied, svint32_t,
		p0 = svcmpge_s32 (p1, z0, z1),
		p0 = svcmpge (p1, z0, z1))

/*
** cmpge_w0_s32:
**	mov	(z[0-9]+\.s), w0
** (
**	cmpge	p0\.s, p1/z, z0\.s, \1
** |
**	cmple	p0\.s, p1/z, \1, z0\.s
** )
**	ret
*/
TEST_COMPARE_ZX (cmpge_w0_s32, svint32_t, int32_t,
		 p0 = svcmpge_n_s32 (p1, z0, x0),
		 p0 = svcmpge (p1, z0, x0))

/*
** cmpge_0_s32:
**	cmpge	p0\.s, p1/z, z0\.s, #0
**	ret
*/
TEST_COMPARE_Z (cmpge_0_s32, svint32_t,
		p0 = svcmpge_n_s32 (p1, z0, 0),
		p0 = svcmpge (p1, z0, 0))

/*
** cmpge_1_s32:
**	cmpge	p0\.s, p1/z, z0\.s, #1
**	ret
*/
TEST_COMPARE_Z (cmpge_1_s32, svint32_t,
		p0 = svcmpge_n_s32 (p1, z0, 1),
		p0 = svcmpge (p1, z0, 1))

/*
** cmpge_15_s32:
**	cmpge	p0\.s, p1/z, z0\.s, #15
**	ret
*/
TEST_COMPARE_Z (cmpge_15_s32, svint32_t,
		p0 = svcmpge_n_s32 (p1, z0, 15),
		p0 = svcmpge (p1, z0, 15))

/*
** cmpge_16_s32:
**	mov	(z[0-9]+\.s), #16
** (
**	cmpge	p0\.s, p1/z, z0\.s, \1
** |
**	cmple	p0\.s, p1/z, \1, z0\.s
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_16_s32, svint32_t,
		p0 = svcmpge_n_s32 (p1, z0, 16),
		p0 = svcmpge (p1, z0, 16))

/*
** cmpge_m1_s32:
**	cmpge	p0\.s, p1/z, z0\.s, #-1
**	ret
*/
TEST_COMPARE_Z (cmpge_m1_s32, svint32_t,
		p0 = svcmpge_n_s32 (p1, z0, -1),
		p0 = svcmpge (p1, z0, -1))

/*
** cmpge_m16_s32:
**	cmpge	p0\.s, p1/z, z0\.s, #-16
**	ret
*/
TEST_COMPARE_Z (cmpge_m16_s32, svint32_t,
		p0 = svcmpge_n_s32 (p1, z0, -16),
		p0 = svcmpge (p1, z0, -16))

/*
** cmpge_m17_s32:
**	mov	(z[0-9]+\.s), #-17
** (
**	cmpge	p0\.s, p1/z, z0\.s, \1
** |
**	cmple	p0\.s, p1/z, \1, z0\.s
** )
**	ret
*/
TEST_COMPARE_Z (cmpge_m17_s32, svint32_t,
		p0 = svcmpge_n_s32 (p1, z0, -17),
		p0 = svcmpge (p1, z0, -17))
