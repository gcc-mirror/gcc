/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** unpkhi_u16_tied1:
**	uunpkhi	z0\.h, z0\.b
**	ret
*/
TEST_DUAL_Z_REV (unpkhi_u16_tied1, svuint16_t, svuint8_t,
		 z0_res = svunpkhi_u16 (z0),
		 z0_res = svunpkhi (z0))

/*
** unpkhi_u16_untied:
**	uunpkhi	z0\.h, z4\.b
**	ret
*/
TEST_DUAL_Z (unpkhi_u16_untied, svuint16_t, svuint8_t,
	     z0 = svunpkhi_u16 (z4),
	     z0 = svunpkhi (z4))
