/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** eorv_x0_u16:
**	eorv	h([0-9]+), p0, z0\.h
**	umov	w0, v\1\.h\[0\]
**	ret
*/
TEST_REDUCTION_X (eorv_x0_u16, uint16_t, svuint16_t,
		  x0 = sveorv_u16 (p0, z0),
		  x0 = sveorv (p0, z0))
