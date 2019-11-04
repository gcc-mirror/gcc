/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** expa_f64_tied1:
**	fexpa	z0\.d, z0\.d
**	ret
*/
TEST_DUAL_Z_REV (expa_f64_tied1, svfloat64_t, svuint64_t,
		 z0_res = svexpa_f64 (z0),
		 z0_res = svexpa (z0))

/*
** expa_f64_untied:
**	fexpa	z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (expa_f64_untied, svfloat64_t, svuint64_t,
	     z0 = svexpa_f64 (z4),
	     z0 = svexpa (z4))
