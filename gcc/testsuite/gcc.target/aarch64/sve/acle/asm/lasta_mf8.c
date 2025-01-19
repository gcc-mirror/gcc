/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lasta_x0_mf8:
**	lasta	b0, p0, z0\.b
**	ret
*/
TEST_REDUCTION_X (lasta_x0_mf8, mfloat8_t, svmfloat8_t,
		  x0 = svlasta_mf8 (p0, z0),
		  x0 = svlasta (p0, z0))
