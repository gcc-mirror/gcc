/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sel_f16_tied1:
**	sel	z0\.h, p0, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (sel_f16_tied1, svfloat16_t,
		z0 = svsel_f16 (p0, z0, z1),
		z0 = svsel (p0, z0, z1))

/*
** sel_f16_tied2:
**	sel	z0\.h, p0, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (sel_f16_tied2, svfloat16_t,
		z0 = svsel_f16 (p0, z1, z0),
		z0 = svsel (p0, z1, z0))

/*
** sel_f16_untied:
**	sel	z0\.h, p0, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (sel_f16_untied, svfloat16_t,
		z0 = svsel_f16 (p0, z1, z2),
		z0 = svsel (p0, z1, z2))
