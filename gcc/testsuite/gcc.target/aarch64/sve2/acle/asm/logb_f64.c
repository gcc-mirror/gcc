/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** logb_f64_m_tied1:
**	flogb	z0\.d, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (logb_f64_m_tied1, svint64_t, svfloat64_t,
	     z0 = svlogb_f64_m (z0, p0, z4),
	     z0 = svlogb_m (z0, p0, z4))

/*
** logb_f64_m_untied:
**	movprfx	z0, z1
**	flogb	z0\.d, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (logb_f64_m_untied, svint64_t, svfloat64_t,
	     z0 = svlogb_f64_m (z1, p0, z4),
	     z0 = svlogb_m (z1, p0, z4))

/*
** logb_f64_z:
**	movprfx	z0\.d, p0/z, z4\.d
**	flogb	z0\.d, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (logb_f64_z, svint64_t, svfloat64_t,
	     z0 = svlogb_f64_z (p0, z4),
	     z0 = svlogb_z (p0, z4))

/*
** logb_f64_x:
**	flogb	z0\.d, p0/m, z4\.d
**	ret
*/
TEST_DUAL_Z (logb_f64_x, svint64_t, svfloat64_t,
	     z0 = svlogb_f64_x (p0, z4),
	     z0 = svlogb_x (p0, z4))

/*
** ptrue_logb_f64_x:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z (ptrue_logb_f64_x, svint64_t, svfloat64_t,
	     z0 = svlogb_f64_x (svptrue_b64 (), z4),
	     z0 = svlogb_x (svptrue_b64 (), z4))
