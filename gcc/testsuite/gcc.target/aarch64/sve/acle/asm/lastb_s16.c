/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lastb_x0_s16:
**	lastb	w0, p0, z0\.h
**	ret
*/
TEST_REDUCTION_X (lastb_x0_s16, int16_t, svint16_t,
		  x0 = svlastb_s16 (p0, z0),
		  x0 = svlastb (p0, z0))
