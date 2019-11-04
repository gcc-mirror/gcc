/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addv_x0_u64:
**	uaddv	(d[0-9]+), p0, z0\.d
**	fmov	x0, \1
**	ret
*/
TEST_REDUCTION_X (addv_x0_u64, uint64_t, svuint64_t,
		  x0 = svaddv_u64 (p0, z0),
		  x0 = svaddv (p0, z0))
