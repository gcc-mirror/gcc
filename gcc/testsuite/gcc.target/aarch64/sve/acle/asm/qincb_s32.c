/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qincb_n_1_s32_tied:
**	sqincb	x0, w0
**	ret
*/
TEST_UNIFORM_S (qincb_n_1_s32_tied, int32_t,
		x0 = svqincb_n_s32 (x0, 1),
		x0 = svqincb (x0, 1))

/*
** qincb_n_1_s32_untied:
**	mov	w0, w1
**	sqincb	x0, w0
**	ret
*/
TEST_UNIFORM_S (qincb_n_1_s32_untied, int32_t,
		x0 = svqincb_n_s32 (x1, 1),
		x0 = svqincb (x1, 1))

/*
** qincb_n_2_s32:
**	sqincb	x0, w0, all, mul #2
**	ret
*/
TEST_UNIFORM_S (qincb_n_2_s32, int32_t,
		x0 = svqincb_n_s32 (x0, 2),
		x0 = svqincb (x0, 2))

/*
** qincb_n_7_s32:
**	sqincb	x0, w0, all, mul #7
**	ret
*/
TEST_UNIFORM_S (qincb_n_7_s32, int32_t,
		x0 = svqincb_n_s32 (x0, 7),
		x0 = svqincb (x0, 7))

/*
** qincb_n_15_s32:
**	sqincb	x0, w0, all, mul #15
**	ret
*/
TEST_UNIFORM_S (qincb_n_15_s32, int32_t,
		x0 = svqincb_n_s32 (x0, 15),
		x0 = svqincb (x0, 15))

/*
** qincb_n_16_s32:
**	sqincb	x0, w0, all, mul #16
**	ret
*/
TEST_UNIFORM_S (qincb_n_16_s32, int32_t,
		x0 = svqincb_n_s32 (x0, 16),
		x0 = svqincb (x0, 16))
