/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qxtnb_s64:
**	sqxtnb	z0\.s, z4\.d
**	ret
*/
TEST_DUAL_Z (qxtnb_s64, svint32_t, svint64_t,
	     z0 = svqxtnb_s64 (z4),
	     z0 = svqxtnb (z4))
