/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lasta_x0_u32:
**	lasta	w0, p0, z0\.s
**	ret
*/
TEST_REDUCTION_X (lasta_x0_u32, uint32_t, svuint32_t,
		  x0 = svlasta_u32 (p0, z0),
		  x0 = svlasta (p0, z0))
