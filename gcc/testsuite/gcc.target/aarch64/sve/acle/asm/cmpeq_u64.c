/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpeq_u64_tied:
**	cmpeq	p0\.d, p0/z, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_COMPARE_Z (cmpeq_u64_tied, svuint64_t,
		p0 = svcmpeq_u64 (p0, z0, z1),
		p0 = svcmpeq (p0, z0, z1))

/*
** cmpeq_u64_untied:
**	cmpeq	p0\.d, p1/z, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_COMPARE_Z (cmpeq_u64_untied, svuint64_t,
		p0 = svcmpeq_u64 (p1, z0, z1),
		p0 = svcmpeq (p1, z0, z1))

/*
** cmpeq_x0_u64:
**	mov	(z[0-9]+\.d), x0
**	cmpeq	p0\.d, p1/z, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_COMPARE_ZX (cmpeq_x0_u64, svuint64_t, uint64_t,
		 p0 = svcmpeq_n_u64 (p1, z0, x0),
		 p0 = svcmpeq (p1, z0, x0))

/*
** cmpeq_0_u64:
**	cmpeq	p0\.d, p1/z, z0\.d, #0
**	ret
*/
TEST_COMPARE_Z (cmpeq_0_u64, svuint64_t,
		p0 = svcmpeq_n_u64 (p1, z0, 0),
		p0 = svcmpeq (p1, z0, 0))

/*
** cmpeq_1_u64:
**	cmpeq	p0\.d, p1/z, z0\.d, #1
**	ret
*/
TEST_COMPARE_Z (cmpeq_1_u64, svuint64_t,
		p0 = svcmpeq_n_u64 (p1, z0, 1),
		p0 = svcmpeq (p1, z0, 1))

/*
** cmpeq_15_u64:
**	cmpeq	p0\.d, p1/z, z0\.d, #15
**	ret
*/
TEST_COMPARE_Z (cmpeq_15_u64, svuint64_t,
		p0 = svcmpeq_n_u64 (p1, z0, 15),
		p0 = svcmpeq (p1, z0, 15))

/*
** cmpeq_16_u64:
**	mov	(z[0-9]+\.d), #16
**	cmpeq	p0\.d, p1/z, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_COMPARE_Z (cmpeq_16_u64, svuint64_t,
		p0 = svcmpeq_n_u64 (p1, z0, 16),
		p0 = svcmpeq (p1, z0, 16))

/*
** cmpeq_m1_u64:
**	cmpeq	p0\.d, p1/z, z0\.d, #-1
**	ret
*/
TEST_COMPARE_Z (cmpeq_m1_u64, svuint64_t,
		p0 = svcmpeq_n_u64 (p1, z0, -1),
		p0 = svcmpeq (p1, z0, -1))

/*
** cmpeq_m16_u64:
**	cmpeq	p0\.d, p1/z, z0\.d, #-16
**	ret
*/
TEST_COMPARE_Z (cmpeq_m16_u64, svuint64_t,
		p0 = svcmpeq_n_u64 (p1, z0, -16),
		p0 = svcmpeq (p1, z0, -16))

/*
** cmpeq_m17_u64:
**	mov	(z[0-9]+\.d), #-17
**	cmpeq	p0\.d, p1/z, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_COMPARE_Z (cmpeq_m17_u64, svuint64_t,
		p0 = svcmpeq_n_u64 (p1, z0, -17),
		p0 = svcmpeq (p1, z0, -17))
