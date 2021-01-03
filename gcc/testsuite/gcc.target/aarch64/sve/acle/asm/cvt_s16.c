/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cvt_s16_f16_m_tied1:
**	fcvtzs	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (cvt_s16_f16_m_tied1, svint16_t, svfloat16_t,
	     z0 = svcvt_s16_f16_m (z0, p0, z4),
	     z0 = svcvt_s16_m (z0, p0, z4))

/*
** cvt_s16_f16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fcvtzs	z0\.h, p0/m, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (cvt_s16_f16_m_tied2, svint16_t, svfloat16_t,
		 z0_res = svcvt_s16_f16_m (z4, p0, z0),
		 z0_res = svcvt_s16_m (z4, p0, z0))

/*
** cvt_s16_f16_m_untied:
**	movprfx	z0, z1
**	fcvtzs	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (cvt_s16_f16_m_untied, svint16_t, svfloat16_t,
	     z0 = svcvt_s16_f16_m (z1, p0, z4),
	     z0 = svcvt_s16_m (z1, p0, z4))

/*
** cvt_s16_f16_z_tied1:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, \1\.h
**	fcvtzs	z0\.h, p0/m, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (cvt_s16_f16_z_tied1, svint16_t, svfloat16_t,
		 z0_res = svcvt_s16_f16_z (p0, z0),
		 z0_res = svcvt_s16_z (p0, z0))

/*
** cvt_s16_f16_z_untied:
**	movprfx	z0\.h, p0/z, z4\.h
**	fcvtzs	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (cvt_s16_f16_z_untied, svint16_t, svfloat16_t,
	     z0 = svcvt_s16_f16_z (p0, z4),
	     z0 = svcvt_s16_z (p0, z4))

/*
** cvt_s16_f16_x_tied1:
**	fcvtzs	z0\.h, p0/m, z0\.h
**	ret
*/
TEST_DUAL_Z_REV (cvt_s16_f16_x_tied1, svint16_t, svfloat16_t,
		 z0_res = svcvt_s16_f16_x (p0, z0),
		 z0_res = svcvt_s16_x (p0, z0))

/*
** cvt_s16_f16_x_untied:
**	movprfx	z0, z4
**	fcvtzs	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (cvt_s16_f16_x_untied, svint16_t, svfloat16_t,
	     z0 = svcvt_s16_f16_x (p0, z4),
	     z0 = svcvt_s16_x (p0, z4))
