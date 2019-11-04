/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qincp_u16_tied:
**	uqincp	z0\.h, p0
**	ret
*/
TEST_UNIFORM_Z (qincp_u16_tied, svuint16_t,
		z0 = svqincp_u16 (z0, p0),
		z0 = svqincp (z0, p0))

/*
** qincp_u16_untied:
**	movprfx	z0, z1
**	uqincp	z0\.h, p0
**	ret
*/
TEST_UNIFORM_Z (qincp_u16_untied, svuint16_t,
		z0 = svqincp_u16 (z1, p0),
		z0 = svqincp (z1, p0))
