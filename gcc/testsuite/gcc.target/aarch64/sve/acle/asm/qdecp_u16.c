/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdecp_u16_tied:
**	uqdecp	z0\.h, p0
**	ret
*/
TEST_UNIFORM_Z (qdecp_u16_tied, svuint16_t,
		z0 = svqdecp_u16 (z0, p0),
		z0 = svqdecp (z0, p0))

/*
** qdecp_u16_untied:
**	movprfx	z0, z1
**	uqdecp	z0\.h, p0
**	ret
*/
TEST_UNIFORM_Z (qdecp_u16_untied, svuint16_t,
		z0 = svqdecp_u16 (z1, p0),
		z0 = svqdecp (z1, p0))
