/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** unpkhi_s64_tied1:
**	sunpkhi	z0\.d, z0\.s
**	ret
*/
TEST_DUAL_Z_REV (unpkhi_s64_tied1, svint64_t, svint32_t,
		 z0_res = svunpkhi_s64 (z0),
		 z0_res = svunpkhi (z0))

/*
** unpkhi_s64_untied:
**	sunpkhi	z0\.d, z4\.s
**	ret
*/
TEST_DUAL_Z (unpkhi_s64_untied, svint64_t, svint32_t,
	     z0 = svunpkhi_s64 (z4),
	     z0 = svunpkhi (z4))
