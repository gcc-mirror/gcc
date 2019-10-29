/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rsqrts_f16_tied1:
**	frsqrts	z0\.h, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (rsqrts_f16_tied1, svfloat16_t,
		z0 = svrsqrts_f16 (z0, z1),
		z0 = svrsqrts (z0, z1))

/*
** rsqrts_f16_tied2:
**	frsqrts	z0\.h, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (rsqrts_f16_tied2, svfloat16_t,
		z0 = svrsqrts_f16 (z1, z0),
		z0 = svrsqrts (z1, z0))

/*
** rsqrts_f16_untied:
**	frsqrts	z0\.h, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (rsqrts_f16_untied, svfloat16_t,
		z0 = svrsqrts_f16 (z1, z2),
		z0 = svrsqrts (z1, z2))
