/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdecp_s16_tied:
**	sqdecp	z0\.h, p0
**	ret
*/
TEST_UNIFORM_Z (qdecp_s16_tied, svint16_t,
		z0 = svqdecp_s16 (z0, p0),
		z0 = svqdecp (z0, p0))

/*
** qdecp_s16_untied:
**	movprfx	z0, z1
**	sqdecp	z0\.h, p0
**	ret
*/
TEST_UNIFORM_Z (qdecp_s16_untied, svint16_t,
		z0 = svqdecp_s16 (z1, p0),
		z0 = svqdecp (z1, p0))
