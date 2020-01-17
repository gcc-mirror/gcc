/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qxtnb_s32:
**	sqxtnb	z0\.h, z4\.s
**	ret
*/
TEST_DUAL_Z (qxtnb_s32, svint16_t, svint32_t,
	     z0 = svqxtnb_s32 (z4),
	     z0 = svqxtnb (z4))
