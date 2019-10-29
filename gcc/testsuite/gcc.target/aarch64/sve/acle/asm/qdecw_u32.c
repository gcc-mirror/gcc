/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdecw_1_u32_tied:
**	uqdecw	z0\.s
**	ret
*/
TEST_UNIFORM_Z (qdecw_1_u32_tied, svuint32_t,
		z0 = svqdecw_u32 (z0, 1),
		z0 = svqdecw (z0, 1))

/*
** qdecw_1_u32_untied:
**	movprfx	z0, z1
**	uqdecw	z0\.s
**	ret
*/
TEST_UNIFORM_Z (qdecw_1_u32_untied, svuint32_t,
		z0 = svqdecw_u32 (z1, 1),
		z0 = svqdecw (z1, 1))

/*
** qdecw_2_u32:
**	uqdecw	z0\.s, all, mul #2
**	ret
*/
TEST_UNIFORM_Z (qdecw_2_u32, svuint32_t,
		z0 = svqdecw_u32 (z0, 2),
		z0 = svqdecw (z0, 2))

/*
** qdecw_7_u32:
**	uqdecw	z0\.s, all, mul #7
**	ret
*/
TEST_UNIFORM_Z (qdecw_7_u32, svuint32_t,
		z0 = svqdecw_u32 (z0, 7),
		z0 = svqdecw (z0, 7))

/*
** qdecw_15_u32:
**	uqdecw	z0\.s, all, mul #15
**	ret
*/
TEST_UNIFORM_Z (qdecw_15_u32, svuint32_t,
		z0 = svqdecw_u32 (z0, 15),
		z0 = svqdecw (z0, 15))

/*
** qdecw_16_u32:
**	uqdecw	z0\.s, all, mul #16
**	ret
*/
TEST_UNIFORM_Z (qdecw_16_u32, svuint32_t,
		z0 = svqdecw_u32 (z0, 16),
		z0 = svqdecw (z0, 16))

/*
** qdecw_n_1_u32_tied:
**	uqdecw	w0
**	ret
*/
TEST_UNIFORM_S (qdecw_n_1_u32_tied, uint32_t,
		x0 = svqdecw_n_u32 (x0, 1),
		x0 = svqdecw (x0, 1))

/*
** qdecw_n_1_u32_untied:
**	mov	w0, w1
**	uqdecw	w0
**	ret
*/
TEST_UNIFORM_S (qdecw_n_1_u32_untied, uint32_t,
		x0 = svqdecw_n_u32 (x1, 1),
		x0 = svqdecw (x1, 1))

/*
** qdecw_n_2_u32:
**	uqdecw	w0, all, mul #2
**	ret
*/
TEST_UNIFORM_S (qdecw_n_2_u32, uint32_t,
		x0 = svqdecw_n_u32 (x0, 2),
		x0 = svqdecw (x0, 2))

/*
** qdecw_n_7_u32:
**	uqdecw	w0, all, mul #7
**	ret
*/
TEST_UNIFORM_S (qdecw_n_7_u32, uint32_t,
		x0 = svqdecw_n_u32 (x0, 7),
		x0 = svqdecw (x0, 7))

/*
** qdecw_n_15_u32:
**	uqdecw	w0, all, mul #15
**	ret
*/
TEST_UNIFORM_S (qdecw_n_15_u32, uint32_t,
		x0 = svqdecw_n_u32 (x0, 15),
		x0 = svqdecw (x0, 15))

/*
** qdecw_n_16_u32:
**	uqdecw	w0, all, mul #16
**	ret
*/
TEST_UNIFORM_S (qdecw_n_16_u32, uint32_t,
		x0 = svqdecw_n_u32 (x0, 16),
		x0 = svqdecw (x0, 16))
