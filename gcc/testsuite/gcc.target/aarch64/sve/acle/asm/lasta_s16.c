/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lasta_x0_s16:
**	lasta	w0, p0, z0\.h
**	ret
*/
TEST_REDUCTION_X (lasta_x0_s16, int16_t, svint16_t,
		  x0 = svlasta_s16 (p0, z0),
		  x0 = svlasta (p0, z0))
