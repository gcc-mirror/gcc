/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qincw_1_u32_tied:
**	uqincw	z0\.s
**	ret
*/
TEST_UNIFORM_Z (qincw_1_u32_tied, svuint32_t,
		z0 = svqincw_u32 (z0, 1),
		z0 = svqincw (z0, 1))

/*
** qincw_1_u32_untied:
**	movprfx	z0, z1
**	uqincw	z0\.s
**	ret
*/
TEST_UNIFORM_Z (qincw_1_u32_untied, svuint32_t,
		z0 = svqincw_u32 (z1, 1),
		z0 = svqincw (z1, 1))

/*
** qincw_2_u32:
**	uqincw	z0\.s, all, mul #2
**	ret
*/
TEST_UNIFORM_Z (qincw_2_u32, svuint32_t,
		z0 = svqincw_u32 (z0, 2),
		z0 = svqincw (z0, 2))

/*
** qincw_7_u32:
**	uqincw	z0\.s, all, mul #7
**	ret
*/
TEST_UNIFORM_Z (qincw_7_u32, svuint32_t,
		z0 = svqincw_u32 (z0, 7),
		z0 = svqincw (z0, 7))

/*
** qincw_15_u32:
**	uqincw	z0\.s, all, mul #15
**	ret
*/
TEST_UNIFORM_Z (qincw_15_u32, svuint32_t,
		z0 = svqincw_u32 (z0, 15),
		z0 = svqincw (z0, 15))

/*
** qincw_16_u32:
**	uqincw	z0\.s, all, mul #16
**	ret
*/
TEST_UNIFORM_Z (qincw_16_u32, svuint32_t,
		z0 = svqincw_u32 (z0, 16),
		z0 = svqincw (z0, 16))

/*
** qincw_n_1_u32_tied:
**	uqincw	w0
**	ret
*/
TEST_UNIFORM_S (qincw_n_1_u32_tied, uint32_t,
		x0 = svqincw_n_u32 (x0, 1),
		x0 = svqincw (x0, 1))

/*
** qincw_n_1_u32_untied:
**	mov	w0, w1
**	uqincw	w0
**	ret
*/
TEST_UNIFORM_S (qincw_n_1_u32_untied, uint32_t,
		x0 = svqincw_n_u32 (x1, 1),
		x0 = svqincw (x1, 1))

/*
** qincw_n_2_u32:
**	uqincw	w0, all, mul #2
**	ret
*/
TEST_UNIFORM_S (qincw_n_2_u32, uint32_t,
		x0 = svqincw_n_u32 (x0, 2),
		x0 = svqincw (x0, 2))

/*
** qincw_n_7_u32:
**	uqincw	w0, all, mul #7
**	ret
*/
TEST_UNIFORM_S (qincw_n_7_u32, uint32_t,
		x0 = svqincw_n_u32 (x0, 7),
		x0 = svqincw (x0, 7))

/*
** qincw_n_15_u32:
**	uqincw	w0, all, mul #15
**	ret
*/
TEST_UNIFORM_S (qincw_n_15_u32, uint32_t,
		x0 = svqincw_n_u32 (x0, 15),
		x0 = svqincw (x0, 15))

/*
** qincw_n_16_u32:
**	uqincw	w0, all, mul #16
**	ret
*/
TEST_UNIFORM_S (qincw_n_16_u32, uint32_t,
		x0 = svqincw_n_u32 (x0, 16),
		x0 = svqincw (x0, 16))
