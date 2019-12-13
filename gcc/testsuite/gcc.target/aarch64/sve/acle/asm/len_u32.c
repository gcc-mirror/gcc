/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** len_x0_u32:
**	cntw	x0
**	ret
*/
TEST_REDUCTION_X (len_x0_u32, uint64_t, svuint32_t,
		  x0 = svlen_u32 (z0),
		  x0 = svlen (z0))
