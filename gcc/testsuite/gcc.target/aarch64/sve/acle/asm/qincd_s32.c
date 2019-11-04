/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qincd_n_1_s32_tied:
**	sqincd	x0, w0
**	ret
*/
TEST_UNIFORM_S (qincd_n_1_s32_tied, int32_t,
		x0 = svqincd_n_s32 (x0, 1),
		x0 = svqincd (x0, 1))

/*
** qincd_n_1_s32_untied:
**	mov	w0, w1
**	sqincd	x0, w0
**	ret
*/
TEST_UNIFORM_S (qincd_n_1_s32_untied, int32_t,
		x0 = svqincd_n_s32 (x1, 1),
		x0 = svqincd (x1, 1))

/*
** qincd_n_2_s32:
**	sqincd	x0, w0, all, mul #2
**	ret
*/
TEST_UNIFORM_S (qincd_n_2_s32, int32_t,
		x0 = svqincd_n_s32 (x0, 2),
		x0 = svqincd (x0, 2))

/*
** qincd_n_7_s32:
**	sqincd	x0, w0, all, mul #7
**	ret
*/
TEST_UNIFORM_S (qincd_n_7_s32, int32_t,
		x0 = svqincd_n_s32 (x0, 7),
		x0 = svqincd (x0, 7))

/*
** qincd_n_15_s32:
**	sqincd	x0, w0, all, mul #15
**	ret
*/
TEST_UNIFORM_S (qincd_n_15_s32, int32_t,
		x0 = svqincd_n_s32 (x0, 15),
		x0 = svqincd (x0, 15))

/*
** qincd_n_16_s32:
**	sqincd	x0, w0, all, mul #16
**	ret
*/
TEST_UNIFORM_S (qincd_n_16_s32, int32_t,
		x0 = svqincd_n_s32 (x0, 16),
		x0 = svqincd (x0, 16))
