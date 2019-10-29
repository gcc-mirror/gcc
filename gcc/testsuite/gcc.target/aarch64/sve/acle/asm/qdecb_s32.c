/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdecb_n_1_s32_tied:
**	sqdecb	x0, w0
**	ret
*/
TEST_UNIFORM_S (qdecb_n_1_s32_tied, int32_t,
		x0 = svqdecb_n_s32 (x0, 1),
		x0 = svqdecb (x0, 1))

/*
** qdecb_n_1_s32_untied:
**	mov	w0, w1
**	sqdecb	x0, w0
**	ret
*/
TEST_UNIFORM_S (qdecb_n_1_s32_untied, int32_t,
		x0 = svqdecb_n_s32 (x1, 1),
		x0 = svqdecb (x1, 1))

/*
** qdecb_n_2_s32:
**	sqdecb	x0, w0, all, mul #2
**	ret
*/
TEST_UNIFORM_S (qdecb_n_2_s32, int32_t,
		x0 = svqdecb_n_s32 (x0, 2),
		x0 = svqdecb (x0, 2))

/*
** qdecb_n_7_s32:
**	sqdecb	x0, w0, all, mul #7
**	ret
*/
TEST_UNIFORM_S (qdecb_n_7_s32, int32_t,
		x0 = svqdecb_n_s32 (x0, 7),
		x0 = svqdecb (x0, 7))

/*
** qdecb_n_15_s32:
**	sqdecb	x0, w0, all, mul #15
**	ret
*/
TEST_UNIFORM_S (qdecb_n_15_s32, int32_t,
		x0 = svqdecb_n_s32 (x0, 15),
		x0 = svqdecb (x0, 15))

/*
** qdecb_n_16_s32:
**	sqdecb	x0, w0, all, mul #16
**	ret
*/
TEST_UNIFORM_S (qdecb_n_16_s32, int32_t,
		x0 = svqdecb_n_s32 (x0, 16),
		x0 = svqdecb (x0, 16))
