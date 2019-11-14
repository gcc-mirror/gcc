/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rev_u16_tied1:
**	rev	z0\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (rev_u16_tied1, svuint16_t,
		z0 = svrev_u16 (z0),
		z0 = svrev (z0))

/*
** rev_u16_untied:
**	rev	z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (rev_u16_untied, svuint16_t,
		z0 = svrev_u16 (z1),
		z0 = svrev (z1))
