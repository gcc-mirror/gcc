/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** unpkhi_s16_tied1:
**	sunpkhi	z0\.h, z0\.b
**	ret
*/
TEST_DUAL_Z_REV (unpkhi_s16_tied1, svint16_t, svint8_t,
		 z0_res = svunpkhi_s16 (z0),
		 z0_res = svunpkhi (z0))

/*
** unpkhi_s16_untied:
**	sunpkhi	z0\.h, z4\.b
**	ret
*/
TEST_DUAL_Z (unpkhi_s16_untied, svint16_t, svint8_t,
	     z0 = svunpkhi_s16 (z4),
	     z0 = svunpkhi (z4))
