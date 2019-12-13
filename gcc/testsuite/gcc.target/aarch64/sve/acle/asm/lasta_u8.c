/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lasta_x0_u8:
**	lasta	w0, p0, z0\.b
**	ret
*/
TEST_REDUCTION_X (lasta_x0_u8, uint8_t, svuint8_t,
		  x0 = svlasta_u8 (p0, z0),
		  x0 = svlasta (p0, z0))
