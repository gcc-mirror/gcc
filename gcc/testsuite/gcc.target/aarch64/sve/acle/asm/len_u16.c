/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** len_x0_u16:
**	cnth	x0
**	ret
*/
TEST_REDUCTION_X (len_x0_u16, uint64_t, svuint16_t,
		  x0 = svlen_u16 (z0),
		  x0 = svlen (z0))
