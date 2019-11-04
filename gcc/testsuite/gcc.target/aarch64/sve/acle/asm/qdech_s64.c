/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdech_n_1_s64_tied:
**	sqdech	x0
**	ret
*/
TEST_UNIFORM_S (qdech_n_1_s64_tied, int64_t,
		x0 = svqdech_n_s64 (x0, 1),
		x0 = svqdech (x0, 1))

/*
** qdech_n_1_s64_untied:
**	mov	x0, x1
**	sqdech	x0
**	ret
*/
TEST_UNIFORM_S (qdech_n_1_s64_untied, int64_t,
		x0 = svqdech_n_s64 (x1, 1),
		x0 = svqdech (x1, 1))

/*
** qdech_n_2_s64:
**	sqdech	x0, all, mul #2
**	ret
*/
TEST_UNIFORM_S (qdech_n_2_s64, int64_t,
		x0 = svqdech_n_s64 (x0, 2),
		x0 = svqdech (x0, 2))

/*
** qdech_n_7_s64:
**	sqdech	x0, all, mul #7
**	ret
*/
TEST_UNIFORM_S (qdech_n_7_s64, int64_t,
		x0 = svqdech_n_s64 (x0, 7),
		x0 = svqdech (x0, 7))

/*
** qdech_n_15_s64:
**	sqdech	x0, all, mul #15
**	ret
*/
TEST_UNIFORM_S (qdech_n_15_s64, int64_t,
		x0 = svqdech_n_s64 (x0, 15),
		x0 = svqdech (x0, 15))

/*
** qdech_n_16_s64:
**	sqdech	x0, all, mul #16
**	ret
*/
TEST_UNIFORM_S (qdech_n_16_s64, int64_t,
		x0 = svqdech_n_s64 (x0, 16),
		x0 = svqdech (x0, 16))
