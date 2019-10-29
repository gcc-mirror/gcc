/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** unpklo_u64_tied1:
**	uunpklo	z0\.d, z0\.s
**	ret
*/
TEST_DUAL_Z_REV (unpklo_u64_tied1, svuint64_t, svuint32_t,
		 z0_res = svunpklo_u64 (z0),
		 z0_res = svunpklo (z0))

/*
** unpklo_u64_untied:
**	uunpklo	z0\.d, z4\.s
**	ret
*/
TEST_DUAL_Z (unpklo_u64_untied, svuint64_t, svuint32_t,
	     z0 = svunpklo_u64 (z4),
	     z0 = svunpklo (z4))
