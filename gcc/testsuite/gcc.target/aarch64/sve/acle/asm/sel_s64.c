/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sel_s64_tied1:
**	sel	z0\.d, p0, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (sel_s64_tied1, svint64_t,
		z0 = svsel_s64 (p0, z0, z1),
		z0 = svsel (p0, z0, z1))

/*
** sel_s64_tied2:
**	sel	z0\.d, p0, z1\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (sel_s64_tied2, svint64_t,
		z0 = svsel_s64 (p0, z1, z0),
		z0 = svsel (p0, z1, z0))

/*
** sel_s64_untied:
**	sel	z0\.d, p0, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (sel_s64_untied, svint64_t,
		z0 = svsel_s64 (p0, z1, z2),
		z0 = svsel (p0, z1, z2))
