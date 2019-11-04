/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdecw_1_s32_tied:
**	sqdecw	z0\.s
**	ret
*/
TEST_UNIFORM_Z (qdecw_1_s32_tied, svint32_t,
		z0 = svqdecw_s32 (z0, 1),
		z0 = svqdecw (z0, 1))

/*
** qdecw_1_s32_untied:
**	movprfx	z0, z1
**	sqdecw	z0\.s
**	ret
*/
TEST_UNIFORM_Z (qdecw_1_s32_untied, svint32_t,
		z0 = svqdecw_s32 (z1, 1),
		z0 = svqdecw (z1, 1))

/*
** qdecw_2_s32:
**	sqdecw	z0\.s, all, mul #2
**	ret
*/
TEST_UNIFORM_Z (qdecw_2_s32, svint32_t,
		z0 = svqdecw_s32 (z0, 2),
		z0 = svqdecw (z0, 2))

/*
** qdecw_7_s32:
**	sqdecw	z0\.s, all, mul #7
**	ret
*/
TEST_UNIFORM_Z (qdecw_7_s32, svint32_t,
		z0 = svqdecw_s32 (z0, 7),
		z0 = svqdecw (z0, 7))

/*
** qdecw_15_s32:
**	sqdecw	z0\.s, all, mul #15
**	ret
*/
TEST_UNIFORM_Z (qdecw_15_s32, svint32_t,
		z0 = svqdecw_s32 (z0, 15),
		z0 = svqdecw (z0, 15))

/*
** qdecw_16_s32:
**	sqdecw	z0\.s, all, mul #16
**	ret
*/
TEST_UNIFORM_Z (qdecw_16_s32, svint32_t,
		z0 = svqdecw_s32 (z0, 16),
		z0 = svqdecw (z0, 16))

/*
** qdecw_n_1_s32_tied:
**	sqdecw	x0, w0
**	ret
*/
TEST_UNIFORM_S (qdecw_n_1_s32_tied, int32_t,
		x0 = svqdecw_n_s32 (x0, 1),
		x0 = svqdecw (x0, 1))

/*
** qdecw_n_1_s32_untied:
**	mov	w0, w1
**	sqdecw	x0, w0
**	ret
*/
TEST_UNIFORM_S (qdecw_n_1_s32_untied, int32_t,
		x0 = svqdecw_n_s32 (x1, 1),
		x0 = svqdecw (x1, 1))

/*
** qdecw_n_2_s32:
**	sqdecw	x0, w0, all, mul #2
**	ret
*/
TEST_UNIFORM_S (qdecw_n_2_s32, int32_t,
		x0 = svqdecw_n_s32 (x0, 2),
		x0 = svqdecw (x0, 2))

/*
** qdecw_n_7_s32:
**	sqdecw	x0, w0, all, mul #7
**	ret
*/
TEST_UNIFORM_S (qdecw_n_7_s32, int32_t,
		x0 = svqdecw_n_s32 (x0, 7),
		x0 = svqdecw (x0, 7))

/*
** qdecw_n_15_s32:
**	sqdecw	x0, w0, all, mul #15
**	ret
*/
TEST_UNIFORM_S (qdecw_n_15_s32, int32_t,
		x0 = svqdecw_n_s32 (x0, 15),
		x0 = svqdecw (x0, 15))

/*
** qdecw_n_16_s32:
**	sqdecw	x0, w0, all, mul #16
**	ret
*/
TEST_UNIFORM_S (qdecw_n_16_s32, int32_t,
		x0 = svqdecw_n_s32 (x0, 16),
		x0 = svqdecw (x0, 16))
