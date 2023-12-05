/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** tssel_f64_tied1:
**	ftssel	z0\.d, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (tssel_f64_tied1, svfloat64_t, svuint64_t,
	     z0 = svtssel_f64 (z0, z4),
	     z0 = svtssel (z0, z4))

/*
** tssel_f64_tied2:
**	ftssel	z0\.d, z4\.d, z0\.d
**	ret
*/
TEST_DUAL_Z_REV (tssel_f64_tied2, svfloat64_t, svuint64_t,
		 z0_res = svtssel_f64 (z4, z0),
		 z0_res = svtssel (z4, z0))

/*
** tssel_f64_untied:
**	ftssel	z0\.d, z1\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (tssel_f64_untied, svfloat64_t, svuint64_t,
	     z0 = svtssel_f64 (z1, z4),
	     z0 = svtssel (z1, z4))
