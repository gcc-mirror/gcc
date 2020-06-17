/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** logb_f32_m_tied1:
**	flogb	z0\.s, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (logb_f32_m_tied1, svint32_t, svfloat32_t,
	     z0 = svlogb_f32_m (z0, p0, z4),
	     z0 = svlogb_m (z0, p0, z4))

/*
** logb_f32_m_untied:
**	movprfx	z0, z1
**	flogb	z0\.s, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (logb_f32_m_untied, svint32_t, svfloat32_t,
	     z0 = svlogb_f32_m (z1, p0, z4),
	     z0 = svlogb_m (z1, p0, z4))

/*
** logb_f32_z:
**	movprfx	z0\.s, p0/z, z4\.s
**	flogb	z0\.s, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (logb_f32_z, svint32_t, svfloat32_t,
	     z0 = svlogb_f32_z (p0, z4),
	     z0 = svlogb_z (p0, z4))

/*
** logb_f32_x:
**	flogb	z0\.s, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (logb_f32_x, svint32_t, svfloat32_t,
	     z0 = svlogb_f32_x (p0, z4),
	     z0 = svlogb_x (p0, z4))

/*
** ptrue_logb_f32_x:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z (ptrue_logb_f32_x, svint32_t, svfloat32_t,
	     z0 = svlogb_f32_x (svptrue_b32 (), z4),
	     z0 = svlogb_x (svptrue_b32 (), z4))
