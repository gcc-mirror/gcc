/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qxtnb_u16:
**	uqxtnb	z0\.b, z4\.h
**	ret
*/
TEST_DUAL_Z (qxtnb_u16, svuint8_t, svuint16_t,
	     z0 = svqxtnb_u16 (z4),
	     z0 = svqxtnb (z4))
