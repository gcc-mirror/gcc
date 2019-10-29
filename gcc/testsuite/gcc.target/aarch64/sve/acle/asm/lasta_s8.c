/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lasta_x0_s8:
**	lasta	w0, p0, z0\.b
**	ret
*/
TEST_REDUCTION_X (lasta_x0_s8, int8_t, svint8_t,
		  x0 = svlasta_s8 (p0, z0),
		  x0 = svlasta (p0, z0))
