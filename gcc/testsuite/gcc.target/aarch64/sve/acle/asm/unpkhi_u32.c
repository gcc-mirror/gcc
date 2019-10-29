/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** unpkhi_u32_tied1:
**	uunpkhi	z0\.s, z0\.h
**	ret
*/
TEST_DUAL_Z_REV (unpkhi_u32_tied1, svuint32_t, svuint16_t,
		 z0_res = svunpkhi_u32 (z0),
		 z0_res = svunpkhi (z0))

/*
** unpkhi_u32_untied:
**	uunpkhi	z0\.s, z4\.h
**	ret
*/
TEST_DUAL_Z (unpkhi_u32_untied, svuint32_t, svuint16_t,
	     z0 = svunpkhi_u32 (z4),
	     z0 = svunpkhi (z4))
