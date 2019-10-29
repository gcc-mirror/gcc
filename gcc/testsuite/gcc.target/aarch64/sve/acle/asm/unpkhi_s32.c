/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** unpkhi_s32_tied1:
**	sunpkhi	z0\.s, z0\.h
**	ret
*/
TEST_DUAL_Z_REV (unpkhi_s32_tied1, svint32_t, svint16_t,
		 z0_res = svunpkhi_s32 (z0),
		 z0_res = svunpkhi (z0))

/*
** unpkhi_s32_untied:
**	sunpkhi	z0\.s, z4\.h
**	ret
*/
TEST_DUAL_Z (unpkhi_s32_untied, svint32_t, svint16_t,
	     z0 = svunpkhi_s32 (z4),
	     z0 = svunpkhi (z4))
