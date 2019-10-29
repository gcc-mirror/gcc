/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qincb_n_1_u64_tied:
**	uqincb	x0
**	ret
*/
TEST_UNIFORM_S (qincb_n_1_u64_tied, uint64_t,
		x0 = svqincb_n_u64 (x0, 1),
		x0 = svqincb (x0, 1))

/*
** qincb_n_1_u64_untied:
**	mov	x0, x1
**	uqincb	x0
**	ret
*/
TEST_UNIFORM_S (qincb_n_1_u64_untied, uint64_t,
		x0 = svqincb_n_u64 (x1, 1),
		x0 = svqincb (x1, 1))

/*
** qincb_n_2_u64:
**	uqincb	x0, all, mul #2
**	ret
*/
TEST_UNIFORM_S (qincb_n_2_u64, uint64_t,
		x0 = svqincb_n_u64 (x0, 2),
		x0 = svqincb (x0, 2))

/*
** qincb_n_7_u64:
**	uqincb	x0, all, mul #7
**	ret
*/
TEST_UNIFORM_S (qincb_n_7_u64, uint64_t,
		x0 = svqincb_n_u64 (x0, 7),
		x0 = svqincb (x0, 7))

/*
** qincb_n_15_u64:
**	uqincb	x0, all, mul #15
**	ret
*/
TEST_UNIFORM_S (qincb_n_15_u64, uint64_t,
		x0 = svqincb_n_u64 (x0, 15),
		x0 = svqincb (x0, 15))

/*
** qincb_n_16_u64:
**	uqincb	x0, all, mul #16
**	ret
*/
TEST_UNIFORM_S (qincb_n_16_u64, uint64_t,
		x0 = svqincb_n_u64 (x0, 16),
		x0 = svqincb (x0, 16))
