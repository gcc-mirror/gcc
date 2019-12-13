/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** recpe_f16_tied1:
**	frecpe	z0\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (recpe_f16_tied1, svfloat16_t,
		z0 = svrecpe_f16 (z0),
		z0 = svrecpe (z0))

/*
** recpe_f16_untied:
**	frecpe	z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (recpe_f16_untied, svfloat16_t,
		z0 = svrecpe_f16 (z1),
		z0 = svrecpe (z1))
