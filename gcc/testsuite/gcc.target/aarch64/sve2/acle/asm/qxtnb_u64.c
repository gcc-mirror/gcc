/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qxtnb_u64:
**	uqxtnb	z0\.s, z4\.d
**	ret
*/
TEST_DUAL_Z (qxtnb_u64, svuint32_t, svuint64_t,
	     z0 = svqxtnb_u64 (z4),
	     z0 = svqxtnb (z4))
