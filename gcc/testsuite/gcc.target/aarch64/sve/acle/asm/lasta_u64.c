/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lasta_x0_u64:
**	lasta	x0, p0, z0\.d
**	ret
*/
TEST_REDUCTION_X (lasta_x0_u64, uint64_t, svuint64_t,
		  x0 = svlasta_u64 (p0, z0),
		  x0 = svlasta (p0, z0))
