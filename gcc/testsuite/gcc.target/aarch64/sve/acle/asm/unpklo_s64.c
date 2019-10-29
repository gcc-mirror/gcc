/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** unpklo_s64_tied1:
**	sunpklo	z0\.d, z0\.s
**	ret
*/
TEST_DUAL_Z_REV (unpklo_s64_tied1, svint64_t, svint32_t,
		 z0_res = svunpklo_s64 (z0),
		 z0_res = svunpklo (z0))

/*
** unpklo_s64_untied:
**	sunpklo	z0\.d, z4\.s
**	ret
*/
TEST_DUAL_Z (unpklo_s64_untied, svint64_t, svint32_t,
	     z0 = svunpklo_s64 (z4),
	     z0 = svunpklo (z4))
