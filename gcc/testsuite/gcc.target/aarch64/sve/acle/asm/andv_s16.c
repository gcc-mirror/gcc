/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** andv_x0_s16:
**	andv	h([0-9]+), p0, z0\.h
**	umov	w0, v\1\.h\[0\]
**	ret
*/
TEST_REDUCTION_X (andv_x0_s16, int16_t, svint16_t,
		  x0 = svandv_s16 (p0, z0),
		  x0 = svandv (p0, z0))
