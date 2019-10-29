/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdecd_1_s64_tied:
**	sqdecd	z0\.d
**	ret
*/
TEST_UNIFORM_Z (qdecd_1_s64_tied, svint64_t,
		z0 = svqdecd_s64 (z0, 1),
		z0 = svqdecd (z0, 1))

/*
** qdecd_1_s64_untied:
**	movprfx	z0, z1
**	sqdecd	z0\.d
**	ret
*/
TEST_UNIFORM_Z (qdecd_1_s64_untied, svint64_t,
		z0 = svqdecd_s64 (z1, 1),
		z0 = svqdecd (z1, 1))

/*
** qdecd_2_s64:
**	sqdecd	z0\.d, all, mul #2
**	ret
*/
TEST_UNIFORM_Z (qdecd_2_s64, svint64_t,
		z0 = svqdecd_s64 (z0, 2),
		z0 = svqdecd (z0, 2))

/*
** qdecd_7_s64:
**	sqdecd	z0\.d, all, mul #7
**	ret
*/
TEST_UNIFORM_Z (qdecd_7_s64, svint64_t,
		z0 = svqdecd_s64 (z0, 7),
		z0 = svqdecd (z0, 7))

/*
** qdecd_15_s64:
**	sqdecd	z0\.d, all, mul #15
**	ret
*/
TEST_UNIFORM_Z (qdecd_15_s64, svint64_t,
		z0 = svqdecd_s64 (z0, 15),
		z0 = svqdecd (z0, 15))

/*
** qdecd_16_s64:
**	sqdecd	z0\.d, all, mul #16
**	ret
*/
TEST_UNIFORM_Z (qdecd_16_s64, svint64_t,
		z0 = svqdecd_s64 (z0, 16),
		z0 = svqdecd (z0, 16))

/*
** qdecd_n_1_s64_tied:
**	sqdecd	x0
**	ret
*/
TEST_UNIFORM_S (qdecd_n_1_s64_tied, int64_t,
		x0 = svqdecd_n_s64 (x0, 1),
		x0 = svqdecd (x0, 1))

/*
** qdecd_n_1_s64_untied:
**	mov	x0, x1
**	sqdecd	x0
**	ret
*/
TEST_UNIFORM_S (qdecd_n_1_s64_untied, int64_t,
		x0 = svqdecd_n_s64 (x1, 1),
		x0 = svqdecd (x1, 1))

/*
** qdecd_n_2_s64:
**	sqdecd	x0, all, mul #2
**	ret
*/
TEST_UNIFORM_S (qdecd_n_2_s64, int64_t,
		x0 = svqdecd_n_s64 (x0, 2),
		x0 = svqdecd (x0, 2))

/*
** qdecd_n_7_s64:
**	sqdecd	x0, all, mul #7
**	ret
*/
TEST_UNIFORM_S (qdecd_n_7_s64, int64_t,
		x0 = svqdecd_n_s64 (x0, 7),
		x0 = svqdecd (x0, 7))

/*
** qdecd_n_15_s64:
**	sqdecd	x0, all, mul #15
**	ret
*/
TEST_UNIFORM_S (qdecd_n_15_s64, int64_t,
		x0 = svqdecd_n_s64 (x0, 15),
		x0 = svqdecd (x0, 15))

/*
** qdecd_n_16_s64:
**	sqdecd	x0, all, mul #16
**	ret
*/
TEST_UNIFORM_S (qdecd_n_16_s64, int64_t,
		x0 = svqdecd_n_s64 (x0, 16),
		x0 = svqdecd (x0, 16))
