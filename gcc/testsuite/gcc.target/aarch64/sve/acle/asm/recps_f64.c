/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** recps_f64_tied1:
**	frecps	z0\.d, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (recps_f64_tied1, svfloat64_t,
		z0 = svrecps_f64 (z0, z1),
		z0 = svrecps (z0, z1))

/*
** recps_f64_tied2:
**	frecps	z0\.d, z1\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (recps_f64_tied2, svfloat64_t,
		z0 = svrecps_f64 (z1, z0),
		z0 = svrecps (z1, z0))

/*
** recps_f64_untied:
**	frecps	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (recps_f64_untied, svfloat64_t,
		z0 = svrecps_f64 (z1, z2),
		z0 = svrecps (z1, z2))
