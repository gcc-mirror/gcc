/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** minv_x0_u32:
**	uminv	(s[0-9]+), p0, z0\.s
**	fmov	w0, \1
**	ret
*/
TEST_REDUCTION_X (minv_x0_u32, uint32_t, svuint32_t,
		  x0 = svminv_u32 (p0, z0),
		  x0 = svminv (p0, z0))
