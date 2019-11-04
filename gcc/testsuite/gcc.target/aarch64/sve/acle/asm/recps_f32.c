/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** recps_f32_tied1:
**	frecps	z0\.s, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (recps_f32_tied1, svfloat32_t,
		z0 = svrecps_f32 (z0, z1),
		z0 = svrecps (z0, z1))

/*
** recps_f32_tied2:
**	frecps	z0\.s, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (recps_f32_tied2, svfloat32_t,
		z0 = svrecps_f32 (z1, z0),
		z0 = svrecps (z1, z0))

/*
** recps_f32_untied:
**	frecps	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (recps_f32_untied, svfloat32_t,
		z0 = svrecps_f32 (z1, z2),
		z0 = svrecps (z1, z2))
