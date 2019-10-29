/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** unpklo_s32_tied1:
**	sunpklo	z0\.s, z0\.h
**	ret
*/
TEST_DUAL_Z_REV (unpklo_s32_tied1, svint32_t, svint16_t,
		 z0_res = svunpklo_s32 (z0),
		 z0_res = svunpklo (z0))

/*
** unpklo_s32_untied:
**	sunpklo	z0\.s, z4\.h
**	ret
*/
TEST_DUAL_Z (unpklo_s32_untied, svint32_t, svint16_t,
	     z0 = svunpklo_s32 (z4),
	     z0 = svunpklo (z4))
