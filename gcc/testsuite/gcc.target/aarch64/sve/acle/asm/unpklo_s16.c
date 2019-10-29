/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** unpklo_s16_tied1:
**	sunpklo	z0\.h, z0\.b
**	ret
*/
TEST_DUAL_Z_REV (unpklo_s16_tied1, svint16_t, svint8_t,
		 z0_res = svunpklo_s16 (z0),
		 z0_res = svunpklo (z0))

/*
** unpklo_s16_untied:
**	sunpklo	z0\.h, z4\.b
**	ret
*/
TEST_DUAL_Z (unpklo_s16_untied, svint16_t, svint8_t,
	     z0 = svunpklo_s16 (z4),
	     z0 = svunpklo (z4))
