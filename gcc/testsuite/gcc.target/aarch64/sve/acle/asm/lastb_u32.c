/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lastb_x0_u32:
**	lastb	w0, p0, z0\.s
**	ret
*/
TEST_REDUCTION_X (lastb_x0_u32, uint32_t, svuint32_t,
		  x0 = svlastb_u32 (p0, z0),
		  x0 = svlastb (p0, z0))
