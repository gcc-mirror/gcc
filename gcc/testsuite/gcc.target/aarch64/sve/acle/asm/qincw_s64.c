/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qincw_n_1_s64_tied:
**	sqincw	x0
**	ret
*/
TEST_UNIFORM_S (qincw_n_1_s64_tied, int64_t,
		x0 = svqincw_n_s64 (x0, 1),
		x0 = svqincw (x0, 1))

/*
** qincw_n_1_s64_untied:
**	mov	x0, x1
**	sqincw	x0
**	ret
*/
TEST_UNIFORM_S (qincw_n_1_s64_untied, int64_t,
		x0 = svqincw_n_s64 (x1, 1),
		x0 = svqincw (x1, 1))

/*
** qincw_n_2_s64:
**	sqincw	x0, all, mul #2
**	ret
*/
TEST_UNIFORM_S (qincw_n_2_s64, int64_t,
		x0 = svqincw_n_s64 (x0, 2),
		x0 = svqincw (x0, 2))

/*
** qincw_n_7_s64:
**	sqincw	x0, all, mul #7
**	ret
*/
TEST_UNIFORM_S (qincw_n_7_s64, int64_t,
		x0 = svqincw_n_s64 (x0, 7),
		x0 = svqincw (x0, 7))

/*
** qincw_n_15_s64:
**	sqincw	x0, all, mul #15
**	ret
*/
TEST_UNIFORM_S (qincw_n_15_s64, int64_t,
		x0 = svqincw_n_s64 (x0, 15),
		x0 = svqincw (x0, 15))

/*
** qincw_n_16_s64:
**	sqincw	x0, all, mul #16
**	ret
*/
TEST_UNIFORM_S (qincw_n_16_s64, int64_t,
		x0 = svqincw_n_s64 (x0, 16),
		x0 = svqincw (x0, 16))
