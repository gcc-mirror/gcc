/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdech_1_s16_tied:
**	sqdech	z0\.h
**	ret
*/
TEST_UNIFORM_Z (qdech_1_s16_tied, svint16_t,
		z0 = svqdech_s16 (z0, 1),
		z0 = svqdech (z0, 1))

/*
** qdech_1_s16_untied:
**	movprfx	z0, z1
**	sqdech	z0\.h
**	ret
*/
TEST_UNIFORM_Z (qdech_1_s16_untied, svint16_t,
		z0 = svqdech_s16 (z1, 1),
		z0 = svqdech (z1, 1))

/*
** qdech_2_s16:
**	sqdech	z0\.h, all, mul #2
**	ret
*/
TEST_UNIFORM_Z (qdech_2_s16, svint16_t,
		z0 = svqdech_s16 (z0, 2),
		z0 = svqdech (z0, 2))

/*
** qdech_7_s16:
**	sqdech	z0\.h, all, mul #7
**	ret
*/
TEST_UNIFORM_Z (qdech_7_s16, svint16_t,
		z0 = svqdech_s16 (z0, 7),
		z0 = svqdech (z0, 7))

/*
** qdech_15_s16:
**	sqdech	z0\.h, all, mul #15
**	ret
*/
TEST_UNIFORM_Z (qdech_15_s16, svint16_t,
		z0 = svqdech_s16 (z0, 15),
		z0 = svqdech (z0, 15))

/*
** qdech_16_s16:
**	sqdech	z0\.h, all, mul #16
**	ret
*/
TEST_UNIFORM_Z (qdech_16_s16, svint16_t,
		z0 = svqdech_s16 (z0, 16),
		z0 = svqdech (z0, 16))
