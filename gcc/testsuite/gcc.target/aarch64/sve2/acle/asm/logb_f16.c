/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** logb_f16_m_tied1:
**	flogb	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (logb_f16_m_tied1, svint16_t, svfloat16_t,
	     z0 = svlogb_f16_m (z0, p0, z4),
	     z0 = svlogb_m (z0, p0, z4))

/*
** logb_f16_m_untied:
**	movprfx	z0, z1
**	flogb	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (logb_f16_m_untied, svint16_t, svfloat16_t,
	     z0 = svlogb_f16_m (z1, p0, z4),
	     z0 = svlogb_m (z1, p0, z4))

/*
** logb_f16_z:
**	movprfx	z0\.h, p0/z, z4\.h
**	flogb	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (logb_f16_z, svint16_t, svfloat16_t,
	     z0 = svlogb_f16_z (p0, z4),
	     z0 = svlogb_z (p0, z4))

/*
** logb_f16_x:
**	movprfx	z0, z4
**	flogb	z0\.h, p0/m, z4\.h
**	ret
*/
TEST_DUAL_Z (logb_f16_x, svint16_t, svfloat16_t,
	     z0 = svlogb_f16_x (p0, z4),
	     z0 = svlogb_x (p0, z4))

/*
** ptrue_logb_f16_x:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z (ptrue_logb_f16_x, svint16_t, svfloat16_t,
	     z0 = svlogb_f16_x (svptrue_b16 (), z4),
	     z0 = svlogb_x (svptrue_b16 (), z4))
