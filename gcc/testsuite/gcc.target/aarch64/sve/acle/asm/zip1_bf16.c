/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** zip1_bf16_tied1:
**	zip1	z0\.h, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (zip1_bf16_tied1, svbfloat16_t,
		z0 = svzip1_bf16 (z0, z1),
		z0 = svzip1 (z0, z1))

/*
** zip1_bf16_tied2:
**	zip1	z0\.h, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (zip1_bf16_tied2, svbfloat16_t,
		z0 = svzip1_bf16 (z1, z0),
		z0 = svzip1 (z1, z0))

/*
** zip1_bf16_untied:
**	zip1	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (zip1_bf16_untied, svbfloat16_t,
		z0 = svzip1_bf16 (z1, z2),
		z0 = svzip1 (z1, z2))
