/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qincd_1_u64_tied:
**	uqincd	z0\.d
**	ret
*/
TEST_UNIFORM_Z (qincd_1_u64_tied, svuint64_t,
		z0 = svqincd_u64 (z0, 1),
		z0 = svqincd (z0, 1))

/*
** qincd_1_u64_untied:
**	movprfx	z0, z1
**	uqincd	z0\.d
**	ret
*/
TEST_UNIFORM_Z (qincd_1_u64_untied, svuint64_t,
		z0 = svqincd_u64 (z1, 1),
		z0 = svqincd (z1, 1))

/*
** qincd_2_u64:
**	uqincd	z0\.d, all, mul #2
**	ret
*/
TEST_UNIFORM_Z (qincd_2_u64, svuint64_t,
		z0 = svqincd_u64 (z0, 2),
		z0 = svqincd (z0, 2))

/*
** qincd_7_u64:
**	uqincd	z0\.d, all, mul #7
**	ret
*/
TEST_UNIFORM_Z (qincd_7_u64, svuint64_t,
		z0 = svqincd_u64 (z0, 7),
		z0 = svqincd (z0, 7))

/*
** qincd_15_u64:
**	uqincd	z0\.d, all, mul #15
**	ret
*/
TEST_UNIFORM_Z (qincd_15_u64, svuint64_t,
		z0 = svqincd_u64 (z0, 15),
		z0 = svqincd (z0, 15))

/*
** qincd_16_u64:
**	uqincd	z0\.d, all, mul #16
**	ret
*/
TEST_UNIFORM_Z (qincd_16_u64, svuint64_t,
		z0 = svqincd_u64 (z0, 16),
		z0 = svqincd (z0, 16))

/*
** qincd_n_1_u64_tied:
**	uqincd	x0
**	ret
*/
TEST_UNIFORM_S (qincd_n_1_u64_tied, uint64_t,
		x0 = svqincd_n_u64 (x0, 1),
		x0 = svqincd (x0, 1))

/*
** qincd_n_1_u64_untied:
**	mov	x0, x1
**	uqincd	x0
**	ret
*/
TEST_UNIFORM_S (qincd_n_1_u64_untied, uint64_t,
		x0 = svqincd_n_u64 (x1, 1),
		x0 = svqincd (x1, 1))

/*
** qincd_n_2_u64:
**	uqincd	x0, all, mul #2
**	ret
*/
TEST_UNIFORM_S (qincd_n_2_u64, uint64_t,
		x0 = svqincd_n_u64 (x0, 2),
		x0 = svqincd (x0, 2))

/*
** qincd_n_7_u64:
**	uqincd	x0, all, mul #7
**	ret
*/
TEST_UNIFORM_S (qincd_n_7_u64, uint64_t,
		x0 = svqincd_n_u64 (x0, 7),
		x0 = svqincd (x0, 7))

/*
** qincd_n_15_u64:
**	uqincd	x0, all, mul #15
**	ret
*/
TEST_UNIFORM_S (qincd_n_15_u64, uint64_t,
		x0 = svqincd_n_u64 (x0, 15),
		x0 = svqincd (x0, 15))

/*
** qincd_n_16_u64:
**	uqincd	x0, all, mul #16
**	ret
*/
TEST_UNIFORM_S (qincd_n_16_u64, uint64_t,
		x0 = svqincd_n_u64 (x0, 16),
		x0 = svqincd (x0, 16))
