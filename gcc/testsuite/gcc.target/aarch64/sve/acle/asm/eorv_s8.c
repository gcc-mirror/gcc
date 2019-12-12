/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** eorv_x0_s8:
**	eorv	b([0-9]+), p0, z0\.b
**	umov	w0, v\1\.b\[0\]
**	ret
*/
TEST_REDUCTION_X (eorv_x0_s8, int8_t, svint8_t,
		  x0 = sveorv_s8 (p0, z0),
		  x0 = sveorv (p0, z0))
