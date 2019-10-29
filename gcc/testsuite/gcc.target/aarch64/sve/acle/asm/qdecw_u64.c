/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdecw_n_1_u64_tied:
**	uqdecw	x0
**	ret
*/
TEST_UNIFORM_S (qdecw_n_1_u64_tied, uint64_t,
		x0 = svqdecw_n_u64 (x0, 1),
		x0 = svqdecw (x0, 1))

/*
** qdecw_n_1_u64_untied:
**	mov	x0, x1
**	uqdecw	x0
**	ret
*/
TEST_UNIFORM_S (qdecw_n_1_u64_untied, uint64_t,
		x0 = svqdecw_n_u64 (x1, 1),
		x0 = svqdecw (x1, 1))

/*
** qdecw_n_2_u64:
**	uqdecw	x0, all, mul #2
**	ret
*/
TEST_UNIFORM_S (qdecw_n_2_u64, uint64_t,
		x0 = svqdecw_n_u64 (x0, 2),
		x0 = svqdecw (x0, 2))

/*
** qdecw_n_7_u64:
**	uqdecw	x0, all, mul #7
**	ret
*/
TEST_UNIFORM_S (qdecw_n_7_u64, uint64_t,
		x0 = svqdecw_n_u64 (x0, 7),
		x0 = svqdecw (x0, 7))

/*
** qdecw_n_15_u64:
**	uqdecw	x0, all, mul #15
**	ret
*/
TEST_UNIFORM_S (qdecw_n_15_u64, uint64_t,
		x0 = svqdecw_n_u64 (x0, 15),
		x0 = svqdecw (x0, 15))

/*
** qdecw_n_16_u64:
**	uqdecw	x0, all, mul #16
**	ret
*/
TEST_UNIFORM_S (qdecw_n_16_u64, uint64_t,
		x0 = svqdecw_n_u64 (x0, 16),
		x0 = svqdecw (x0, 16))
