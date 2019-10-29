/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdecd_n_1_s32_tied:
**	sqdecd	x0, w0
**	ret
*/
TEST_UNIFORM_S (qdecd_n_1_s32_tied, int32_t,
		x0 = svqdecd_n_s32 (x0, 1),
		x0 = svqdecd (x0, 1))

/*
** qdecd_n_1_s32_untied:
**	mov	w0, w1
**	sqdecd	x0, w0
**	ret
*/
TEST_UNIFORM_S (qdecd_n_1_s32_untied, int32_t,
		x0 = svqdecd_n_s32 (x1, 1),
		x0 = svqdecd (x1, 1))

/*
** qdecd_n_2_s32:
**	sqdecd	x0, w0, all, mul #2
**	ret
*/
TEST_UNIFORM_S (qdecd_n_2_s32, int32_t,
		x0 = svqdecd_n_s32 (x0, 2),
		x0 = svqdecd (x0, 2))

/*
** qdecd_n_7_s32:
**	sqdecd	x0, w0, all, mul #7
**	ret
*/
TEST_UNIFORM_S (qdecd_n_7_s32, int32_t,
		x0 = svqdecd_n_s32 (x0, 7),
		x0 = svqdecd (x0, 7))

/*
** qdecd_n_15_s32:
**	sqdecd	x0, w0, all, mul #15
**	ret
*/
TEST_UNIFORM_S (qdecd_n_15_s32, int32_t,
		x0 = svqdecd_n_s32 (x0, 15),
		x0 = svqdecd (x0, 15))

/*
** qdecd_n_16_s32:
**	sqdecd	x0, w0, all, mul #16
**	ret
*/
TEST_UNIFORM_S (qdecd_n_16_s32, int32_t,
		x0 = svqdecd_n_s32 (x0, 16),
		x0 = svqdecd (x0, 16))
