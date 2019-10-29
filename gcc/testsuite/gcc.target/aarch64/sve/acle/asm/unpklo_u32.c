/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** unpklo_u32_tied1:
**	uunpklo	z0\.s, z0\.h
**	ret
*/
TEST_DUAL_Z_REV (unpklo_u32_tied1, svuint32_t, svuint16_t,
		 z0_res = svunpklo_u32 (z0),
		 z0_res = svunpklo (z0))

/*
** unpklo_u32_untied:
**	uunpklo	z0\.s, z4\.h
**	ret
*/
TEST_DUAL_Z (unpklo_u32_untied, svuint32_t, svuint16_t,
	     z0 = svunpklo_u32 (z4),
	     z0 = svunpklo (z4))
