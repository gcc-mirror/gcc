/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qxtnb_s16:
**	sqxtnb	z0\.b, z4\.h
**	ret
*/
TEST_DUAL_Z (qxtnb_s16, svint8_t, svint16_t,
	     z0 = svqxtnb_s16 (z4),
	     z0 = svqxtnb (z4))
