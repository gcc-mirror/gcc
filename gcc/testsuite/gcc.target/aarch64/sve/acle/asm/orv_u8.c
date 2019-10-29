/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** orv_x0_u8:
**	orv	b([0-9]+), p0, z0\.b
**	umov	w0, v\1\.b\[0\]
**	ret
*/
TEST_REDUCTION_X (orv_x0_u8, uint8_t, svuint8_t,
		  x0 = svorv_u8 (p0, z0),
		  x0 = svorv (p0, z0))
