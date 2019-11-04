/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rev_u8_tied1:
**	rev	z0\.b, z0\.b
**	ret
*/
TEST_UNIFORM_Z (rev_u8_tied1, svuint8_t,
		z0 = svrev_u8 (z0),
		z0 = svrev (z0))

/*
** rev_u8_untied:
**	rev	z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (rev_u8_untied, svuint8_t,
		z0 = svrev_u8 (z1),
		z0 = svrev (z1))
