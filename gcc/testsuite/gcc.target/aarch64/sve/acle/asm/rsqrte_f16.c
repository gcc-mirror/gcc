/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rsqrte_f16_tied1:
**	frsqrte	z0\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (rsqrte_f16_tied1, svfloat16_t,
		z0 = svrsqrte_f16 (z0),
		z0 = svrsqrte (z0))

/*
** rsqrte_f16_untied:
**	frsqrte	z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (rsqrte_f16_untied, svfloat16_t,
		z0 = svrsqrte_f16 (z1),
		z0 = svrsqrte (z1))
