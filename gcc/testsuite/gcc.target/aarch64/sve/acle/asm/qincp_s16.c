/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qincp_s16_tied:
**	sqincp	z0\.h, p0
**	ret
*/
TEST_UNIFORM_Z (qincp_s16_tied, svint16_t,
		z0 = svqincp_s16 (z0, p0),
		z0 = svqincp (z0, p0))

/*
** qincp_s16_untied:
**	movprfx	z0, z1
**	sqincp	z0\.h, p0
**	ret
*/
TEST_UNIFORM_Z (qincp_s16_untied, svint16_t,
		z0 = svqincp_s16 (z1, p0),
		z0 = svqincp (z1, p0))
