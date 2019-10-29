/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdecb_n_1_s64_tied:
**	sqdecb	x0
**	ret
*/
TEST_UNIFORM_S (qdecb_n_1_s64_tied, int64_t,
		x0 = svqdecb_n_s64 (x0, 1),
		x0 = svqdecb (x0, 1))

/*
** qdecb_n_1_s64_untied:
**	mov	x0, x1
**	sqdecb	x0
**	ret
*/
TEST_UNIFORM_S (qdecb_n_1_s64_untied, int64_t,
		x0 = svqdecb_n_s64 (x1, 1),
		x0 = svqdecb (x1, 1))

/*
** qdecb_n_2_s64:
**	sqdecb	x0, all, mul #2
**	ret
*/
TEST_UNIFORM_S (qdecb_n_2_s64, int64_t,
		x0 = svqdecb_n_s64 (x0, 2),
		x0 = svqdecb (x0, 2))

/*
** qdecb_n_7_s64:
**	sqdecb	x0, all, mul #7
**	ret
*/
TEST_UNIFORM_S (qdecb_n_7_s64, int64_t,
		x0 = svqdecb_n_s64 (x0, 7),
		x0 = svqdecb (x0, 7))

/*
** qdecb_n_15_s64:
**	sqdecb	x0, all, mul #15
**	ret
*/
TEST_UNIFORM_S (qdecb_n_15_s64, int64_t,
		x0 = svqdecb_n_s64 (x0, 15),
		x0 = svqdecb (x0, 15))

/*
** qdecb_n_16_s64:
**	sqdecb	x0, all, mul #16
**	ret
*/
TEST_UNIFORM_S (qdecb_n_16_s64, int64_t,
		x0 = svqdecb_n_s64 (x0, 16),
		x0 = svqdecb (x0, 16))
