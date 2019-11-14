/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** tssel_f16_tied1:
**	ftssel	z0\.h, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (tssel_f16_tied1, svfloat16_t, svuint16_t,
	     z0 = svtssel_f16 (z0, z4),
	     z0 = svtssel (z0, z4))

/*
** tssel_f16_tied2:
**	ftssel	z0\.h, z4\.h, z0\.h
**	ret
*/
TEST_DUAL_Z_REV (tssel_f16_tied2, svfloat16_t, svuint16_t,
		 z0_res = svtssel_f16 (z4, z0),
		 z0_res = svtssel (z4, z0))

/*
** tssel_f16_untied:
**	ftssel	z0\.h, z1\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (tssel_f16_untied, svfloat16_t, svuint16_t,
	     z0 = svtssel_f16 (z1, z4),
	     z0 = svtssel (z1, z4))
