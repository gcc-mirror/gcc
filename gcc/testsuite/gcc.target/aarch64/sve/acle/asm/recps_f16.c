/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** recps_f16_tied1:
**	frecps	z0\.h, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (recps_f16_tied1, svfloat16_t,
		z0 = svrecps_f16 (z0, z1),
		z0 = svrecps (z0, z1))

/*
** recps_f16_tied2:
**	frecps	z0\.h, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (recps_f16_tied2, svfloat16_t,
		z0 = svrecps_f16 (z1, z0),
		z0 = svrecps (z1, z0))

/*
** recps_f16_untied:
**	frecps	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (recps_f16_untied, svfloat16_t,
		z0 = svrecps_f16 (z1, z2),
		z0 = svrecps (z1, z2))
