/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** recpe_f64_tied1:
**	frecpe	z0\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (recpe_f64_tied1, svfloat64_t,
		z0 = svrecpe_f64 (z0),
		z0 = svrecpe (z0))

/*
** recpe_f64_untied:
**	frecpe	z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (recpe_f64_untied, svfloat64_t,
		z0 = svrecpe_f64 (z1),
		z0 = svrecpe (z1))
