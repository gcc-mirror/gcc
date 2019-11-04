/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sel_u16_tied1:
**	sel	z0\.h, p0, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (sel_u16_tied1, svuint16_t,
		z0 = svsel_u16 (p0, z0, z1),
		z0 = svsel (p0, z0, z1))

/*
** sel_u16_tied2:
**	sel	z0\.h, p0, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (sel_u16_tied2, svuint16_t,
		z0 = svsel_u16 (p0, z1, z0),
		z0 = svsel (p0, z1, z0))

/*
** sel_u16_untied:
**	sel	z0\.h, p0, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (sel_u16_untied, svuint16_t,
		z0 = svsel_u16 (p0, z1, z2),
		z0 = svsel (p0, z1, z2))
