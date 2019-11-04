/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** unpklo_u16_tied1:
**	uunpklo	z0\.h, z0\.b
**	ret
*/
TEST_DUAL_Z_REV (unpklo_u16_tied1, svuint16_t, svuint8_t,
		 z0_res = svunpklo_u16 (z0),
		 z0_res = svunpklo (z0))

/*
** unpklo_u16_untied:
**	uunpklo	z0\.h, z4\.b
**	ret
*/
TEST_DUAL_Z (unpklo_u16_untied, svuint16_t, svuint8_t,
	     z0 = svunpklo_u16 (z4),
	     z0 = svunpklo (z4))
