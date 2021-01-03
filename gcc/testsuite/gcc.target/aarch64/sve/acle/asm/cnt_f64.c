/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cnt_f64_m_tied1:
**	cnt	z0\.d, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cnt_f64_m_tied1, svuint64_t, svfloat64_t,
	     z0 = svcnt_f64_m (z0, p0, z4),
	     z0 = svcnt_m (z0, p0, z4))

/*
** cnt_f64_m_untied:
**	movprfx	z0, z1
**	cnt	z0\.d, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cnt_f64_m_untied, svuint64_t, svfloat64_t,
	     z0 = svcnt_f64_m (z1, p0, z4),
	     z0 = svcnt_m (z1, p0, z4))

/*
** cnt_f64_z:
**	movprfx	z0\.d, p0/z, z4\.d
**	cnt	z0\.d, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cnt_f64_z, svuint64_t, svfloat64_t,
	     z0 = svcnt_f64_z (p0, z4),
	     z0 = svcnt_z (p0, z4))

/*
** cnt_f64_x:
**	movprfx	z0, z4
**	cnt	z0\.d, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (cnt_f64_x, svuint64_t, svfloat64_t,
	     z0 = svcnt_f64_x (p0, z4),
	     z0 = svcnt_x (p0, z4))

/*
** ptrue_cnt_f64_x:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z (ptrue_cnt_f64_x, svuint64_t, svfloat64_t,
	     z0 = svcnt_f64_x (svptrue_b64 (), z4),
	     z0 = svcnt_x (svptrue_b64 (), z4))
