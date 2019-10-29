/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmple_s32_tied:
** (
**	cmpge	p0\.s, p0/z, z1\.s, z0\.s
** |
**	cmple	p0\.s, p0/z, z0\.s, z1\.s
** )
**	ret
*/
TEST_COMPARE_Z (cmple_s32_tied, svint32_t,
		p0 = svcmple_s32 (p0, z0, z1),
		p0 = svcmple (p0, z0, z1))

/*
** cmple_s32_untied:
** (
**	cmpge	p0\.s, p1/z, z1\.s, z0\.s
** |
**	cmple	p0\.s, p1/z, z0\.s, z1\.s
** )
**	ret
*/
TEST_COMPARE_Z (cmple_s32_untied, svint32_t,
		p0 = svcmple_s32 (p1, z0, z1),
		p0 = svcmple (p1, z0, z1))

/*
** cmple_w0_s32:
**	mov	(z[0-9]+\.s), w0
** (
**	cmpge	p0\.s, p1/z, \1, z0\.s
** |
**	cmple	p0\.s, p1/z, z0\.s, \1
** )
**	ret
*/
TEST_COMPARE_ZX (cmple_w0_s32, svint32_t, int32_t,
		 p0 = svcmple_n_s32 (p1, z0, x0),
		 p0 = svcmple (p1, z0, x0))

/*
** cmple_0_s32:
**	cmple	p0\.s, p1/z, z0\.s, #0
**	ret
*/
TEST_COMPARE_Z (cmple_0_s32, svint32_t,
		p0 = svcmple_n_s32 (p1, z0, 0),
		p0 = svcmple (p1, z0, 0))

/*
** cmple_1_s32:
**	cmple	p0\.s, p1/z, z0\.s, #1
**	ret
*/
TEST_COMPARE_Z (cmple_1_s32, svint32_t,
		p0 = svcmple_n_s32 (p1, z0, 1),
		p0 = svcmple (p1, z0, 1))

/*
** cmple_15_s32:
**	cmple	p0\.s, p1/z, z0\.s, #15
**	ret
*/
TEST_COMPARE_Z (cmple_15_s32, svint32_t,
		p0 = svcmple_n_s32 (p1, z0, 15),
		p0 = svcmple (p1, z0, 15))

/*
** cmple_16_s32:
**	mov	(z[0-9]+\.s), #16
** (
**	cmpge	p0\.s, p1/z, \1, z0\.s
** |
**	cmple	p0\.s, p1/z, z0\.s, \1
** )
**	ret
*/
TEST_COMPARE_Z (cmple_16_s32, svint32_t,
		p0 = svcmple_n_s32 (p1, z0, 16),
		p0 = svcmple (p1, z0, 16))

/*
** cmple_m1_s32:
**	cmple	p0\.s, p1/z, z0\.s, #-1
**	ret
*/
TEST_COMPARE_Z (cmple_m1_s32, svint32_t,
		p0 = svcmple_n_s32 (p1, z0, -1),
		p0 = svcmple (p1, z0, -1))

/*
** cmple_m16_s32:
**	cmple	p0\.s, p1/z, z0\.s, #-16
**	ret
*/
TEST_COMPARE_Z (cmple_m16_s32, svint32_t,
		p0 = svcmple_n_s32 (p1, z0, -16),
		p0 = svcmple (p1, z0, -16))

/*
** cmple_m17_s32:
**	mov	(z[0-9]+\.s), #-17
** (
**	cmpge	p0\.s, p1/z, \1, z0\.s
** |
**	cmple	p0\.s, p1/z, z0\.s, \1
** )
**	ret
*/
TEST_COMPARE_Z (cmple_m17_s32, svint32_t,
		p0 = svcmple_n_s32 (p1, z0, -17),
		p0 = svcmple (p1, z0, -17))
