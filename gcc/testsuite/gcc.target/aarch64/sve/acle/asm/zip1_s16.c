/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** zip1_s16_tied1:
**	zip1	z0\.h, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (zip1_s16_tied1, svint16_t,
		z0 = svzip1_s16 (z0, z1),
		z0 = svzip1 (z0, z1))

/*
** zip1_s16_tied2:
**	zip1	z0\.h, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (zip1_s16_tied2, svint16_t,
		z0 = svzip1_s16 (z1, z0),
		z0 = svzip1 (z1, z0))

/*
** zip1_s16_untied:
**	zip1	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (zip1_s16_untied, svint16_t,
		z0 = svzip1_s16 (z1, z2),
		z0 = svzip1 (z1, z2))
