/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qincb_n_1_u32_tied:
**	uqincb	w0
**	ret
*/
TEST_UNIFORM_S (qincb_n_1_u32_tied, uint32_t,
		x0 = svqincb_n_u32 (x0, 1),
		x0 = svqincb (x0, 1))

/*
** qincb_n_1_u32_untied:
**	mov	w0, w1
**	uqincb	w0
**	ret
*/
TEST_UNIFORM_S (qincb_n_1_u32_untied, uint32_t,
		x0 = svqincb_n_u32 (x1, 1),
		x0 = svqincb (x1, 1))

/*
** qincb_n_2_u32:
**	uqincb	w0, all, mul #2
**	ret
*/
TEST_UNIFORM_S (qincb_n_2_u32, uint32_t,
		x0 = svqincb_n_u32 (x0, 2),
		x0 = svqincb (x0, 2))

/*
** qincb_n_7_u32:
**	uqincb	w0, all, mul #7
**	ret
*/
TEST_UNIFORM_S (qincb_n_7_u32, uint32_t,
		x0 = svqincb_n_u32 (x0, 7),
		x0 = svqincb (x0, 7))

/*
** qincb_n_15_u32:
**	uqincb	w0, all, mul #15
**	ret
*/
TEST_UNIFORM_S (qincb_n_15_u32, uint32_t,
		x0 = svqincb_n_u32 (x0, 15),
		x0 = svqincb (x0, 15))

/*
** qincb_n_16_u32:
**	uqincb	w0, all, mul #16
**	ret
*/
TEST_UNIFORM_S (qincb_n_16_u32, uint32_t,
		x0 = svqincb_n_u32 (x0, 16),
		x0 = svqincb (x0, 16))
