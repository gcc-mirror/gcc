/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qxtnb_u32:
**	uqxtnb	z0\.h, z4\.s
**	ret
*/
TEST_DUAL_Z (qxtnb_u32, svuint16_t, svuint32_t,
	     z0 = svqxtnb_u32 (z4),
	     z0 = svqxtnb (z4))
