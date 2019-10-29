/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** addv_x0_u8:
**	uaddv	(d[0-9]+), p0, z0\.b
**	fmov	x0, \1
**	ret
*/
TEST_REDUCTION_X (addv_x0_u8, uint64_t, svuint8_t,
		  x0 = svaddv_u8 (p0, z0),
		  x0 = svaddv (p0, z0))
