/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qincw_1_s32_tied:
**	sqincw	z0\.s
**	ret
*/
TEST_UNIFORM_Z (qincw_1_s32_tied, svint32_t,
		z0 = svqincw_s32 (z0, 1),
		z0 = svqincw (z0, 1))

/*
** qincw_1_s32_untied:
**	movprfx	z0, z1
**	sqincw	z0\.s
**	ret
*/
TEST_UNIFORM_Z (qincw_1_s32_untied, svint32_t,
		z0 = svqincw_s32 (z1, 1),
		z0 = svqincw (z1, 1))

/*
** qincw_2_s32:
**	sqincw	z0\.s, all, mul #2
**	ret
*/
TEST_UNIFORM_Z (qincw_2_s32, svint32_t,
		z0 = svqincw_s32 (z0, 2),
		z0 = svqincw (z0, 2))

/*
** qincw_7_s32:
**	sqincw	z0\.s, all, mul #7
**	ret
*/
TEST_UNIFORM_Z (qincw_7_s32, svint32_t,
		z0 = svqincw_s32 (z0, 7),
		z0 = svqincw (z0, 7))

/*
** qincw_15_s32:
**	sqincw	z0\.s, all, mul #15
**	ret
*/
TEST_UNIFORM_Z (qincw_15_s32, svint32_t,
		z0 = svqincw_s32 (z0, 15),
		z0 = svqincw (z0, 15))

/*
** qincw_16_s32:
**	sqincw	z0\.s, all, mul #16
**	ret
*/
TEST_UNIFORM_Z (qincw_16_s32, svint32_t,
		z0 = svqincw_s32 (z0, 16),
		z0 = svqincw (z0, 16))

/*
** qincw_n_1_s32_tied:
**	sqincw	x0, w0
**	ret
*/
TEST_UNIFORM_S (qincw_n_1_s32_tied, int32_t,
		x0 = svqincw_n_s32 (x0, 1),
		x0 = svqincw (x0, 1))

/*
** qincw_n_1_s32_untied:
**	mov	w0, w1
**	sqincw	x0, w0
**	ret
*/
TEST_UNIFORM_S (qincw_n_1_s32_untied, int32_t,
		x0 = svqincw_n_s32 (x1, 1),
		x0 = svqincw (x1, 1))

/*
** qincw_n_2_s32:
**	sqincw	x0, w0, all, mul #2
**	ret
*/
TEST_UNIFORM_S (qincw_n_2_s32, int32_t,
		x0 = svqincw_n_s32 (x0, 2),
		x0 = svqincw (x0, 2))

/*
** qincw_n_7_s32:
**	sqincw	x0, w0, all, mul #7
**	ret
*/
TEST_UNIFORM_S (qincw_n_7_s32, int32_t,
		x0 = svqincw_n_s32 (x0, 7),
		x0 = svqincw (x0, 7))

/*
** qincw_n_15_s32:
**	sqincw	x0, w0, all, mul #15
**	ret
*/
TEST_UNIFORM_S (qincw_n_15_s32, int32_t,
		x0 = svqincw_n_s32 (x0, 15),
		x0 = svqincw (x0, 15))

/*
** qincw_n_16_s32:
**	sqincw	x0, w0, all, mul #16
**	ret
*/
TEST_UNIFORM_S (qincw_n_16_s32, int32_t,
		x0 = svqincw_n_s32 (x0, 16),
		x0 = svqincw (x0, 16))
