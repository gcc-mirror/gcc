/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpeq_wide_s8_tied:
**	cmpeq	p0\.b, p0/z, z0\.b, z1\.d
**	ret
*/
TEST_COMPARE_DUAL_Z (cmpeq_wide_s8_tied, svint8_t, svint64_t,
		     p0 = svcmpeq_wide_s8 (p0, z0, z1),
		     p0 = svcmpeq_wide (p0, z0, z1))

/*
** cmpeq_wide_s8_untied:
**	cmpeq	p0\.b, p1/z, z0\.b, z1\.d
**	ret
*/
TEST_COMPARE_DUAL_Z (cmpeq_wide_s8_untied, svint8_t, svint64_t,
		     p0 = svcmpeq_wide_s8 (p1, z0, z1),
		     p0 = svcmpeq_wide (p1, z0, z1))

/*
** cmpeq_wide_x0_s8:
**	mov	(z[0-9]+\.d), x0
**	cmpeq	p0\.b, p1/z, z0\.b, \1
**	ret
*/
TEST_COMPARE_ZX (cmpeq_wide_x0_s8, svint8_t, int64_t,
		 p0 = svcmpeq_wide_n_s8 (p1, z0, x0),
		 p0 = svcmpeq_wide (p1, z0, x0))

/*
** cmpeq_wide_0_s8:
**	cmpeq	p0\.b, p1/z, z0\.b, #0
**	ret
*/
TEST_COMPARE_Z (cmpeq_wide_0_s8, svint8_t,
		p0 = svcmpeq_wide_n_s8 (p1, z0, 0),
		p0 = svcmpeq_wide (p1, z0, 0))

/*
** cmpeq_wide_1_s8:
**	cmpeq	p0\.b, p1/z, z0\.b, #1
**	ret
*/
TEST_COMPARE_Z (cmpeq_wide_1_s8, svint8_t,
		p0 = svcmpeq_wide_n_s8 (p1, z0, 1),
		p0 = svcmpeq_wide (p1, z0, 1))

/*
** cmpeq_wide_15_s8:
**	cmpeq	p0\.b, p1/z, z0\.b, #15
**	ret
*/
TEST_COMPARE_Z (cmpeq_wide_15_s8, svint8_t,
		p0 = svcmpeq_wide_n_s8 (p1, z0, 15),
		p0 = svcmpeq_wide (p1, z0, 15))

/*
** cmpeq_wide_16_s8:
**	mov	(z[0-9]+\.d), #16
**	cmpeq	p0\.b, p1/z, z0\.b, \1
**	ret
*/
TEST_COMPARE_Z (cmpeq_wide_16_s8, svint8_t,
		p0 = svcmpeq_wide_n_s8 (p1, z0, 16),
		p0 = svcmpeq_wide (p1, z0, 16))

/*
** cmpeq_wide_m1_s8:
**	cmpeq	p0\.b, p1/z, z0\.b, #-1
**	ret
*/
TEST_COMPARE_Z (cmpeq_wide_m1_s8, svint8_t,
		p0 = svcmpeq_wide_n_s8 (p1, z0, -1),
		p0 = svcmpeq_wide (p1, z0, -1))

/*
** cmpeq_wide_m16_s8:
**	cmpeq	p0\.b, p1/z, z0\.b, #-16
**	ret
*/
TEST_COMPARE_Z (cmpeq_wide_m16_s8, svint8_t,
		p0 = svcmpeq_wide_n_s8 (p1, z0, -16),
		p0 = svcmpeq_wide (p1, z0, -16))

/*
** cmpeq_wide_m17_s8:
**	mov	(z[0-9]+\.d), #-17
**	cmpeq	p0\.b, p1/z, z0\.b, \1
**	ret
*/
TEST_COMPARE_Z (cmpeq_wide_m17_s8, svint8_t,
		p0 = svcmpeq_wide_n_s8 (p1, z0, -17),
		p0 = svcmpeq_wide (p1, z0, -17))
