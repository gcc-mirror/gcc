/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpeq_wide_s32_tied:
**	cmpeq	p0\.s, p0/z, z0\.s, z1\.d
**	ret
*/
TEST_COMPARE_DUAL_Z (cmpeq_wide_s32_tied, svint32_t, svint64_t,
		     p0 = svcmpeq_wide_s32 (p0, z0, z1),
		     p0 = svcmpeq_wide (p0, z0, z1))

/*
** cmpeq_wide_s32_untied:
**	cmpeq	p0\.s, p1/z, z0\.s, z1\.d
**	ret
*/
TEST_COMPARE_DUAL_Z (cmpeq_wide_s32_untied, svint32_t, svint64_t,
		     p0 = svcmpeq_wide_s32 (p1, z0, z1),
		     p0 = svcmpeq_wide (p1, z0, z1))

/*
** cmpeq_wide_x0_s32:
**	mov	(z[0-9]+\.d), x0
**	cmpeq	p0\.s, p1/z, z0\.s, \1
**	ret
*/
TEST_COMPARE_ZX (cmpeq_wide_x0_s32, svint32_t, int64_t,
		 p0 = svcmpeq_wide_n_s32 (p1, z0, x0),
		 p0 = svcmpeq_wide (p1, z0, x0))

/*
** cmpeq_wide_0_s32:
**	cmpeq	p0\.s, p1/z, z0\.s, #0
**	ret
*/
TEST_COMPARE_Z (cmpeq_wide_0_s32, svint32_t,
		p0 = svcmpeq_wide_n_s32 (p1, z0, 0),
		p0 = svcmpeq_wide (p1, z0, 0))

/*
** cmpeq_wide_1_s32:
**	cmpeq	p0\.s, p1/z, z0\.s, #1
**	ret
*/
TEST_COMPARE_Z (cmpeq_wide_1_s32, svint32_t,
		p0 = svcmpeq_wide_n_s32 (p1, z0, 1),
		p0 = svcmpeq_wide (p1, z0, 1))

/*
** cmpeq_wide_15_s32:
**	cmpeq	p0\.s, p1/z, z0\.s, #15
**	ret
*/
TEST_COMPARE_Z (cmpeq_wide_15_s32, svint32_t,
		p0 = svcmpeq_wide_n_s32 (p1, z0, 15),
		p0 = svcmpeq_wide (p1, z0, 15))

/*
** cmpeq_wide_16_s32:
**	mov	(z[0-9]+\.d), #16
**	cmpeq	p0\.s, p1/z, z0\.s, \1
**	ret
*/
TEST_COMPARE_Z (cmpeq_wide_16_s32, svint32_t,
		p0 = svcmpeq_wide_n_s32 (p1, z0, 16),
		p0 = svcmpeq_wide (p1, z0, 16))

/*
** cmpeq_wide_m1_s32:
**	cmpeq	p0\.s, p1/z, z0\.s, #-1
**	ret
*/
TEST_COMPARE_Z (cmpeq_wide_m1_s32, svint32_t,
		p0 = svcmpeq_wide_n_s32 (p1, z0, -1),
		p0 = svcmpeq_wide (p1, z0, -1))

/*
** cmpeq_wide_m16_s32:
**	cmpeq	p0\.s, p1/z, z0\.s, #-16
**	ret
*/
TEST_COMPARE_Z (cmpeq_wide_m16_s32, svint32_t,
		p0 = svcmpeq_wide_n_s32 (p1, z0, -16),
		p0 = svcmpeq_wide (p1, z0, -16))

/*
** cmpeq_wide_m17_s32:
**	mov	(z[0-9]+\.d), #-17
**	cmpeq	p0\.s, p1/z, z0\.s, \1
**	ret
*/
TEST_COMPARE_Z (cmpeq_wide_m17_s32, svint32_t,
		p0 = svcmpeq_wide_n_s32 (p1, z0, -17),
		p0 = svcmpeq_wide (p1, z0, -17))
