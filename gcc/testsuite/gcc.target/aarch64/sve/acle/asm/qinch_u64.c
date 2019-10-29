/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qinch_n_1_u64_tied:
**	uqinch	x0
**	ret
*/
TEST_UNIFORM_S (qinch_n_1_u64_tied, uint64_t,
		x0 = svqinch_n_u64 (x0, 1),
		x0 = svqinch (x0, 1))

/*
** qinch_n_1_u64_untied:
**	mov	x0, x1
**	uqinch	x0
**	ret
*/
TEST_UNIFORM_S (qinch_n_1_u64_untied, uint64_t,
		x0 = svqinch_n_u64 (x1, 1),
		x0 = svqinch (x1, 1))

/*
** qinch_n_2_u64:
**	uqinch	x0, all, mul #2
**	ret
*/
TEST_UNIFORM_S (qinch_n_2_u64, uint64_t,
		x0 = svqinch_n_u64 (x0, 2),
		x0 = svqinch (x0, 2))

/*
** qinch_n_7_u64:
**	uqinch	x0, all, mul #7
**	ret
*/
TEST_UNIFORM_S (qinch_n_7_u64, uint64_t,
		x0 = svqinch_n_u64 (x0, 7),
		x0 = svqinch (x0, 7))

/*
** qinch_n_15_u64:
**	uqinch	x0, all, mul #15
**	ret
*/
TEST_UNIFORM_S (qinch_n_15_u64, uint64_t,
		x0 = svqinch_n_u64 (x0, 15),
		x0 = svqinch (x0, 15))

/*
** qinch_n_16_u64:
**	uqinch	x0, all, mul #16
**	ret
*/
TEST_UNIFORM_S (qinch_n_16_u64, uint64_t,
		x0 = svqinch_n_u64 (x0, 16),
		x0 = svqinch (x0, 16))
