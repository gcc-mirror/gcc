/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qinch_n_1_u32_tied:
**	uqinch	w0
**	ret
*/
TEST_UNIFORM_S (qinch_n_1_u32_tied, uint32_t,
		x0 = svqinch_n_u32 (x0, 1),
		x0 = svqinch (x0, 1))

/*
** qinch_n_1_u32_untied:
**	mov	w0, w1
**	uqinch	w0
**	ret
*/
TEST_UNIFORM_S (qinch_n_1_u32_untied, uint32_t,
		x0 = svqinch_n_u32 (x1, 1),
		x0 = svqinch (x1, 1))

/*
** qinch_n_2_u32:
**	uqinch	w0, all, mul #2
**	ret
*/
TEST_UNIFORM_S (qinch_n_2_u32, uint32_t,
		x0 = svqinch_n_u32 (x0, 2),
		x0 = svqinch (x0, 2))

/*
** qinch_n_7_u32:
**	uqinch	w0, all, mul #7
**	ret
*/
TEST_UNIFORM_S (qinch_n_7_u32, uint32_t,
		x0 = svqinch_n_u32 (x0, 7),
		x0 = svqinch (x0, 7))

/*
** qinch_n_15_u32:
**	uqinch	w0, all, mul #15
**	ret
*/
TEST_UNIFORM_S (qinch_n_15_u32, uint32_t,
		x0 = svqinch_n_u32 (x0, 15),
		x0 = svqinch (x0, 15))

/*
** qinch_n_16_u32:
**	uqinch	w0, all, mul #16
**	ret
*/
TEST_UNIFORM_S (qinch_n_16_u32, uint32_t,
		x0 = svqinch_n_u32 (x0, 16),
		x0 = svqinch (x0, 16))
