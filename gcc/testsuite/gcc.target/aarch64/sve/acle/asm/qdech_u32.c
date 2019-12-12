/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdech_n_1_u32_tied:
**	uqdech	w0
**	ret
*/
TEST_UNIFORM_S (qdech_n_1_u32_tied, uint32_t,
		x0 = svqdech_n_u32 (x0, 1),
		x0 = svqdech (x0, 1))

/*
** qdech_n_1_u32_untied:
**	mov	w0, w1
**	uqdech	w0
**	ret
*/
TEST_UNIFORM_S (qdech_n_1_u32_untied, uint32_t,
		x0 = svqdech_n_u32 (x1, 1),
		x0 = svqdech (x1, 1))

/*
** qdech_n_2_u32:
**	uqdech	w0, all, mul #2
**	ret
*/
TEST_UNIFORM_S (qdech_n_2_u32, uint32_t,
		x0 = svqdech_n_u32 (x0, 2),
		x0 = svqdech (x0, 2))

/*
** qdech_n_7_u32:
**	uqdech	w0, all, mul #7
**	ret
*/
TEST_UNIFORM_S (qdech_n_7_u32, uint32_t,
		x0 = svqdech_n_u32 (x0, 7),
		x0 = svqdech (x0, 7))

/*
** qdech_n_15_u32:
**	uqdech	w0, all, mul #15
**	ret
*/
TEST_UNIFORM_S (qdech_n_15_u32, uint32_t,
		x0 = svqdech_n_u32 (x0, 15),
		x0 = svqdech (x0, 15))

/*
** qdech_n_16_u32:
**	uqdech	w0, all, mul #16
**	ret
*/
TEST_UNIFORM_S (qdech_n_16_u32, uint32_t,
		x0 = svqdech_n_u32 (x0, 16),
		x0 = svqdech (x0, 16))
