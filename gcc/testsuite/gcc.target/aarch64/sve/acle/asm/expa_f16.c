/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** expa_f16_tied1:
**	fexpa	z0\.h, z0\.h
**	ret
*/
TEST_DUAL_Z_REV (expa_f16_tied1, svfloat16_t, svuint16_t,
		 z0_res = svexpa_f16 (z0),
		 z0_res = svexpa (z0))

/*
** expa_f16_untied:
**	fexpa	z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (expa_f16_untied, svfloat16_t, svuint16_t,
	     z0 = svexpa_f16 (z4),
	     z0 = svexpa (z4))
