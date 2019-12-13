/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** zip1_s8_tied1:
**	zip1	z0\.b, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (zip1_s8_tied1, svint8_t,
		z0 = svzip1_s8 (z0, z1),
		z0 = svzip1 (z0, z1))

/*
** zip1_s8_tied2:
**	zip1	z0\.b, z1\.b, z0\.b
**	ret
*/
TEST_UNIFORM_Z (zip1_s8_tied2, svint8_t,
		z0 = svzip1_s8 (z1, z0),
		z0 = svzip1 (z1, z0))

/*
** zip1_s8_untied:
**	zip1	z0\.b, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (zip1_s8_untied, svint8_t,
		z0 = svzip1_s8 (z1, z2),
		z0 = svzip1 (z1, z2))
