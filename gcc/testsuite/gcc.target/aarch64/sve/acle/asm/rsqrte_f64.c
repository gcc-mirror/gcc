/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rsqrte_f64_tied1:
**	frsqrte	z0\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (rsqrte_f64_tied1, svfloat64_t,
		z0 = svrsqrte_f64 (z0),
		z0 = svrsqrte (z0))

/*
** rsqrte_f64_untied:
**	frsqrte	z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (rsqrte_f64_untied, svfloat64_t,
		z0 = svrsqrte_f64 (z1),
		z0 = svrsqrte (z1))
