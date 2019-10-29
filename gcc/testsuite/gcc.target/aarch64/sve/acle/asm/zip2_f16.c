/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** zip2_f16_tied1:
**	zip2	z0\.h, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (zip2_f16_tied1, svfloat16_t,
		z0 = svzip2_f16 (z0, z1),
		z0 = svzip2 (z0, z1))

/*
** zip2_f16_tied2:
**	zip2	z0\.h, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (zip2_f16_tied2, svfloat16_t,
		z0 = svzip2_f16 (z1, z0),
		z0 = svzip2 (z1, z0))

/*
** zip2_f16_untied:
**	zip2	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (zip2_f16_untied, svfloat16_t,
		z0 = svzip2_f16 (z1, z2),
		z0 = svzip2 (z1, z2))
