/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** unpkhi_u64_tied1:
**	uunpkhi	z0\.d, z0\.s
**	ret
*/
TEST_DUAL_Z_REV (unpkhi_u64_tied1, svuint64_t, svuint32_t,
		 z0_res = svunpkhi_u64 (z0),
		 z0_res = svunpkhi (z0))

/*
** unpkhi_u64_untied:
**	uunpkhi	z0\.d, z4\.s
**	ret
*/
TEST_DUAL_Z (unpkhi_u64_untied, svuint64_t, svuint32_t,
	     z0 = svunpkhi_u64 (z4),
	     z0 = svunpkhi (z4))
