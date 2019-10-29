/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rsqrts_f64_tied1:
**	frsqrts	z0\.d, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (rsqrts_f64_tied1, svfloat64_t,
		z0 = svrsqrts_f64 (z0, z1),
		z0 = svrsqrts (z0, z1))

/*
** rsqrts_f64_tied2:
**	frsqrts	z0\.d, z1\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (rsqrts_f64_tied2, svfloat64_t,
		z0 = svrsqrts_f64 (z1, z0),
		z0 = svrsqrts (z1, z0))

/*
** rsqrts_f64_untied:
**	frsqrts	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (rsqrts_f64_untied, svfloat64_t,
		z0 = svrsqrts_f64 (z1, z2),
		z0 = svrsqrts (z1, z2))
