/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** orv_x0_u64:
**	orv	(d[0-9]+), p0, z0\.d
**	fmov	x0, \1
**	ret
*/
TEST_REDUCTION_X (orv_x0_u64, uint64_t, svuint64_t,
		  x0 = svorv_u64 (p0, z0),
		  x0 = svorv (p0, z0))
