/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** eorv_x0_s32:
**	eorv	(s[0-9]+), p0, z0\.s
**	fmov	w0, \1
**	ret
*/
TEST_REDUCTION_X (eorv_x0_s32, int32_t, svint32_t,
		  x0 = sveorv_s32 (p0, z0),
		  x0 = sveorv (p0, z0))
