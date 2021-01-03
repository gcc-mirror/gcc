/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cnt_f32_m_tied1:
**	cnt	z0\.s, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (cnt_f32_m_tied1, svuint32_t, svfloat32_t,
	     z0 = svcnt_f32_m (z0, p0, z4),
	     z0 = svcnt_m (z0, p0, z4))

/*
** cnt_f32_m_untied:
**	movprfx	z0, z1
**	cnt	z0\.s, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (cnt_f32_m_untied, svuint32_t, svfloat32_t,
	     z0 = svcnt_f32_m (z1, p0, z4),
	     z0 = svcnt_m (z1, p0, z4))

/*
** cnt_f32_z:
**	movprfx	z0\.s, p0/z, z4\.s
**	cnt	z0\.s, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (cnt_f32_z, svuint32_t, svfloat32_t,
	     z0 = svcnt_f32_z (p0, z4),
	     z0 = svcnt_z (p0, z4))

/*
** cnt_f32_x:
**	movprfx	z0, z4
**	cnt	z0\.s, p0/m, z4\.s
**	ret
*/
TEST_DUAL_Z (cnt_f32_x, svuint32_t, svfloat32_t,
	     z0 = svcnt_f32_x (p0, z4),
	     z0 = svcnt_x (p0, z4))

/*
** ptrue_cnt_f32_x:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z (ptrue_cnt_f32_x, svuint32_t, svfloat32_t,
	     z0 = svcnt_f32_x (svptrue_b32 (), z4),
	     z0 = svcnt_x (svptrue_b32 (), z4))
