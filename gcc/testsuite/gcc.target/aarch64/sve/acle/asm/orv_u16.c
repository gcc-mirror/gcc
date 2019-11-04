/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** orv_x0_u16:
**	orv	h([0-9]+), p0, z0\.h
**	umov	w0, v\1\.h\[0\]
**	ret
*/
TEST_REDUCTION_X (orv_x0_u16, uint16_t, svuint16_t,
		  x0 = svorv_u16 (p0, z0),
		  x0 = svorv (p0, z0))
