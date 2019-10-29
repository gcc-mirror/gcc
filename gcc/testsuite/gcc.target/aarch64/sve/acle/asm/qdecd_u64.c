/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdecd_1_u64_tied:
**	uqdecd	z0\.d
**	ret
*/
TEST_UNIFORM_Z (qdecd_1_u64_tied, svuint64_t,
		z0 = svqdecd_u64 (z0, 1),
		z0 = svqdecd (z0, 1))

/*
** qdecd_1_u64_untied:
**	movprfx	z0, z1
**	uqdecd	z0\.d
**	ret
*/
TEST_UNIFORM_Z (qdecd_1_u64_untied, svuint64_t,
		z0 = svqdecd_u64 (z1, 1),
		z0 = svqdecd (z1, 1))

/*
** qdecd_2_u64:
**	uqdecd	z0\.d, all, mul #2
**	ret
*/
TEST_UNIFORM_Z (qdecd_2_u64, svuint64_t,
		z0 = svqdecd_u64 (z0, 2),
		z0 = svqdecd (z0, 2))

/*
** qdecd_7_u64:
**	uqdecd	z0\.d, all, mul #7
**	ret
*/
TEST_UNIFORM_Z (qdecd_7_u64, svuint64_t,
		z0 = svqdecd_u64 (z0, 7),
		z0 = svqdecd (z0, 7))

/*
** qdecd_15_u64:
**	uqdecd	z0\.d, all, mul #15
**	ret
*/
TEST_UNIFORM_Z (qdecd_15_u64, svuint64_t,
		z0 = svqdecd_u64 (z0, 15),
		z0 = svqdecd (z0, 15))

/*
** qdecd_16_u64:
**	uqdecd	z0\.d, all, mul #16
**	ret
*/
TEST_UNIFORM_Z (qdecd_16_u64, svuint64_t,
		z0 = svqdecd_u64 (z0, 16),
		z0 = svqdecd (z0, 16))

/*
** qdecd_n_1_u64_tied:
**	uqdecd	x0
**	ret
*/
TEST_UNIFORM_S (qdecd_n_1_u64_tied, uint64_t,
		x0 = svqdecd_n_u64 (x0, 1),
		x0 = svqdecd (x0, 1))

/*
** qdecd_n_1_u64_untied:
**	mov	x0, x1
**	uqdecd	x0
**	ret
*/
TEST_UNIFORM_S (qdecd_n_1_u64_untied, uint64_t,
		x0 = svqdecd_n_u64 (x1, 1),
		x0 = svqdecd (x1, 1))

/*
** qdecd_n_2_u64:
**	uqdecd	x0, all, mul #2
**	ret
*/
TEST_UNIFORM_S (qdecd_n_2_u64, uint64_t,
		x0 = svqdecd_n_u64 (x0, 2),
		x0 = svqdecd (x0, 2))

/*
** qdecd_n_7_u64:
**	uqdecd	x0, all, mul #7
**	ret
*/
TEST_UNIFORM_S (qdecd_n_7_u64, uint64_t,
		x0 = svqdecd_n_u64 (x0, 7),
		x0 = svqdecd (x0, 7))

/*
** qdecd_n_15_u64:
**	uqdecd	x0, all, mul #15
**	ret
*/
TEST_UNIFORM_S (qdecd_n_15_u64, uint64_t,
		x0 = svqdecd_n_u64 (x0, 15),
		x0 = svqdecd (x0, 15))

/*
** qdecd_n_16_u64:
**	uqdecd	x0, all, mul #16
**	ret
*/
TEST_UNIFORM_S (qdecd_n_16_u64, uint64_t,
		x0 = svqdecd_n_u64 (x0, 16),
		x0 = svqdecd (x0, 16))
