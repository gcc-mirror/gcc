/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpeq_s64_tied:
**	cmpeq	p0\.d, p0/z, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_COMPARE_Z (cmpeq_s64_tied, svint64_t,
		p0 = svcmpeq_s64 (p0, z0, z1),
		p0 = svcmpeq (p0, z0, z1))

/*
** cmpeq_s64_untied:
**	cmpeq	p0\.d, p1/z, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_COMPARE_Z (cmpeq_s64_untied, svint64_t,
		p0 = svcmpeq_s64 (p1, z0, z1),
		p0 = svcmpeq (p1, z0, z1))

/*
** cmpeq_x0_s64:
**	mov	(z[0-9]+\.d), x0
**	cmpeq	p0\.d, p1/z, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_COMPARE_ZX (cmpeq_x0_s64, svint64_t, int64_t,
		 p0 = svcmpeq_n_s64 (p1, z0, x0),
		 p0 = svcmpeq (p1, z0, x0))

/*
** cmpeq_0_s64:
**	cmpeq	p0\.d, p1/z, z0\.d, #0
**	ret
*/
TEST_COMPARE_Z (cmpeq_0_s64, svint64_t,
		p0 = svcmpeq_n_s64 (p1, z0, 0),
		p0 = svcmpeq (p1, z0, 0))

/*
** cmpeq_1_s64:
**	cmpeq	p0\.d, p1/z, z0\.d, #1
**	ret
*/
TEST_COMPARE_Z (cmpeq_1_s64, svint64_t,
		p0 = svcmpeq_n_s64 (p1, z0, 1),
		p0 = svcmpeq (p1, z0, 1))

/*
** cmpeq_15_s64:
**	cmpeq	p0\.d, p1/z, z0\.d, #15
**	ret
*/
TEST_COMPARE_Z (cmpeq_15_s64, svint64_t,
		p0 = svcmpeq_n_s64 (p1, z0, 15),
		p0 = svcmpeq (p1, z0, 15))

/*
** cmpeq_16_s64:
**	mov	(z[0-9]+\.d), #16
**	cmpeq	p0\.d, p1/z, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_COMPARE_Z (cmpeq_16_s64, svint64_t,
		p0 = svcmpeq_n_s64 (p1, z0, 16),
		p0 = svcmpeq (p1, z0, 16))

/*
** cmpeq_m1_s64:
**	cmpeq	p0\.d, p1/z, z0\.d, #-1
**	ret
*/
TEST_COMPARE_Z (cmpeq_m1_s64, svint64_t,
		p0 = svcmpeq_n_s64 (p1, z0, -1),
		p0 = svcmpeq (p1, z0, -1))

/*
** cmpeq_m16_s64:
**	cmpeq	p0\.d, p1/z, z0\.d, #-16
**	ret
*/
TEST_COMPARE_Z (cmpeq_m16_s64, svint64_t,
		p0 = svcmpeq_n_s64 (p1, z0, -16),
		p0 = svcmpeq (p1, z0, -16))

/*
** cmpeq_m17_s64:
**	mov	(z[0-9]+\.d), #-17
**	cmpeq	p0\.d, p1/z, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_COMPARE_Z (cmpeq_m17_s64, svint64_t,
		p0 = svcmpeq_n_s64 (p1, z0, -17),
		p0 = svcmpeq (p1, z0, -17))
