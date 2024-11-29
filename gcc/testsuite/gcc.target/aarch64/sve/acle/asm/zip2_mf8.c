/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** zip2_mf8_tied1:
**	zip2	z0\.b, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (zip2_mf8_tied1, svmfloat8_t,
		z0 = svzip2_mf8 (z0, z1),
		z0 = svzip2 (z0, z1))

/*
** zip2_mf8_tied2:
**	zip2	z0\.b, z1\.b, z0\.b
**	ret
*/
TEST_UNIFORM_Z (zip2_mf8_tied2, svmfloat8_t,
		z0 = svzip2_mf8 (z1, z0),
		z0 = svzip2 (z1, z0))

/*
** zip2_mf8_untied:
**	zip2	z0\.b, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (zip2_mf8_untied, svmfloat8_t,
		z0 = svzip2_mf8 (z1, z2),
		z0 = svzip2 (z1, z2))
