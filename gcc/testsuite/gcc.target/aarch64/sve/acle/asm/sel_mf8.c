/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sel_mf8_tied1:
**	sel	z0\.b, p0, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (sel_mf8_tied1, svmfloat8_t,
		z0 = svsel_mf8 (p0, z0, z1),
		z0 = svsel (p0, z0, z1))

/*
** sel_mf8_tied2:
**	sel	z0\.b, p0, z1\.b, z0\.b
**	ret
*/
TEST_UNIFORM_Z (sel_mf8_tied2, svmfloat8_t,
		z0 = svsel_mf8 (p0, z1, z0),
		z0 = svsel (p0, z1, z0))

/*
** sel_mf8_untied:
**	sel	z0\.b, p0, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (sel_mf8_untied, svmfloat8_t,
		z0 = svsel_mf8 (p0, z1, z2),
		z0 = svsel (p0, z1, z2))
