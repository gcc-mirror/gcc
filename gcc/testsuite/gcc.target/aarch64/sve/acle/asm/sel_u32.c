/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sel_u32_tied1:
**	sel	z0\.s, p0, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (sel_u32_tied1, svuint32_t,
		z0 = svsel_u32 (p0, z0, z1),
		z0 = svsel (p0, z0, z1))

/*
** sel_u32_tied2:
**	sel	z0\.s, p0, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (sel_u32_tied2, svuint32_t,
		z0 = svsel_u32 (p0, z1, z0),
		z0 = svsel (p0, z1, z0))

/*
** sel_u32_untied:
**	sel	z0\.s, p0, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (sel_u32_untied, svuint32_t,
		z0 = svsel_u32 (p0, z1, z2),
		z0 = svsel (p0, z1, z2))
