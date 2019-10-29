/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdech_1_u16_tied:
**	uqdech	z0\.h
**	ret
*/
TEST_UNIFORM_Z (qdech_1_u16_tied, svuint16_t,
		z0 = svqdech_u16 (z0, 1),
		z0 = svqdech (z0, 1))

/*
** qdech_1_u16_untied:
**	movprfx	z0, z1
**	uqdech	z0\.h
**	ret
*/
TEST_UNIFORM_Z (qdech_1_u16_untied, svuint16_t,
		z0 = svqdech_u16 (z1, 1),
		z0 = svqdech (z1, 1))

/*
** qdech_2_u16:
**	uqdech	z0\.h, all, mul #2
**	ret
*/
TEST_UNIFORM_Z (qdech_2_u16, svuint16_t,
		z0 = svqdech_u16 (z0, 2),
		z0 = svqdech (z0, 2))

/*
** qdech_7_u16:
**	uqdech	z0\.h, all, mul #7
**	ret
*/
TEST_UNIFORM_Z (qdech_7_u16, svuint16_t,
		z0 = svqdech_u16 (z0, 7),
		z0 = svqdech (z0, 7))

/*
** qdech_15_u16:
**	uqdech	z0\.h, all, mul #15
**	ret
*/
TEST_UNIFORM_Z (qdech_15_u16, svuint16_t,
		z0 = svqdech_u16 (z0, 15),
		z0 = svqdech (z0, 15))

/*
** qdech_16_u16:
**	uqdech	z0\.h, all, mul #16
**	ret
*/
TEST_UNIFORM_Z (qdech_16_u16, svuint16_t,
		z0 = svqdech_u16 (z0, 16),
		z0 = svqdech (z0, 16))
