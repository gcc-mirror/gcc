/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qincd_1_s64_tied:
**	sqincd	z0\.d
**	ret
*/
TEST_UNIFORM_Z (qincd_1_s64_tied, svint64_t,
		z0 = svqincd_s64 (z0, 1),
		z0 = svqincd (z0, 1))

/*
** qincd_1_s64_untied:
**	movprfx	z0, z1
**	sqincd	z0\.d
**	ret
*/
TEST_UNIFORM_Z (qincd_1_s64_untied, svint64_t,
		z0 = svqincd_s64 (z1, 1),
		z0 = svqincd (z1, 1))

/*
** qincd_2_s64:
**	sqincd	z0\.d, all, mul #2
**	ret
*/
TEST_UNIFORM_Z (qincd_2_s64, svint64_t,
		z0 = svqincd_s64 (z0, 2),
		z0 = svqincd (z0, 2))

/*
** qincd_7_s64:
**	sqincd	z0\.d, all, mul #7
**	ret
*/
TEST_UNIFORM_Z (qincd_7_s64, svint64_t,
		z0 = svqincd_s64 (z0, 7),
		z0 = svqincd (z0, 7))

/*
** qincd_15_s64:
**	sqincd	z0\.d, all, mul #15
**	ret
*/
TEST_UNIFORM_Z (qincd_15_s64, svint64_t,
		z0 = svqincd_s64 (z0, 15),
		z0 = svqincd (z0, 15))

/*
** qincd_16_s64:
**	sqincd	z0\.d, all, mul #16
**	ret
*/
TEST_UNIFORM_Z (qincd_16_s64, svint64_t,
		z0 = svqincd_s64 (z0, 16),
		z0 = svqincd (z0, 16))

/*
** qincd_n_1_s64_tied:
**	sqincd	x0
**	ret
*/
TEST_UNIFORM_S (qincd_n_1_s64_tied, int64_t,
		x0 = svqincd_n_s64 (x0, 1),
		x0 = svqincd (x0, 1))

/*
** qincd_n_1_s64_untied:
**	mov	x0, x1
**	sqincd	x0
**	ret
*/
TEST_UNIFORM_S (qincd_n_1_s64_untied, int64_t,
		x0 = svqincd_n_s64 (x1, 1),
		x0 = svqincd (x1, 1))

/*
** qincd_n_2_s64:
**	sqincd	x0, all, mul #2
**	ret
*/
TEST_UNIFORM_S (qincd_n_2_s64, int64_t,
		x0 = svqincd_n_s64 (x0, 2),
		x0 = svqincd (x0, 2))

/*
** qincd_n_7_s64:
**	sqincd	x0, all, mul #7
**	ret
*/
TEST_UNIFORM_S (qincd_n_7_s64, int64_t,
		x0 = svqincd_n_s64 (x0, 7),
		x0 = svqincd (x0, 7))

/*
** qincd_n_15_s64:
**	sqincd	x0, all, mul #15
**	ret
*/
TEST_UNIFORM_S (qincd_n_15_s64, int64_t,
		x0 = svqincd_n_s64 (x0, 15),
		x0 = svqincd (x0, 15))

/*
** qincd_n_16_s64:
**	sqincd	x0, all, mul #16
**	ret
*/
TEST_UNIFORM_S (qincd_n_16_s64, int64_t,
		x0 = svqincd_n_s64 (x0, 16),
		x0 = svqincd (x0, 16))
