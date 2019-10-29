/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** scale_f32_m_tied1:
**	fscale	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (scale_f32_m_tied1, svfloat32_t, svint32_t,
	     z0 = svscale_f32_m (p0, z0, z4),
	     z0 = svscale_m (p0, z0, z4))

/*
** scale_f32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fscale	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (scale_f32_m_tied2, svfloat32_t, svint32_t,
		 z0_res = svscale_f32_m (p0, z4, z0),
		 z0_res = svscale_m (p0, z4, z0))

/*
** scale_f32_m_untied:
**	movprfx	z0, z1
**	fscale	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (scale_f32_m_untied, svfloat32_t, svint32_t,
	     z0 = svscale_f32_m (p0, z1, z4),
	     z0 = svscale_m (p0, z1, z4))

/*
** scale_w0_f32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	fscale	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (scale_w0_f32_m_tied1, svfloat32_t, int32_t,
		 z0 = svscale_n_f32_m (p0, z0, x0),
		 z0 = svscale_m (p0, z0, x0))

/*
** scale_w0_f32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	fscale	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (scale_w0_f32_m_untied, svfloat32_t, int32_t,
		 z0 = svscale_n_f32_m (p0, z1, x0),
		 z0 = svscale_m (p0, z1, x0))

/*
** scale_3_f32_m_tied1:
**	mov	(z[0-9]+\.s), #3
**	fscale	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (scale_3_f32_m_tied1, svfloat32_t,
		z0 = svscale_n_f32_m (p0, z0, 3),
		z0 = svscale_m (p0, z0, 3))

/*
** scale_3_f32_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.s), #3
**	movprfx	z0, z1
**	fscale	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (scale_3_f32_m_untied, svfloat32_t,
		z0 = svscale_n_f32_m (p0, z1, 3),
		z0 = svscale_m (p0, z1, 3))

/*
** scale_m3_f32_m:
**	mov	(z[0-9]+\.s), #-3
**	fscale	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (scale_m3_f32_m, svfloat32_t,
		z0 = svscale_n_f32_m (p0, z0, -3),
		z0 = svscale_m (p0, z0, -3))

/*
** scale_f32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	fscale	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (scale_f32_z_tied1, svfloat32_t, svint32_t,
	     z0 = svscale_f32_z (p0, z0, z4),
	     z0 = svscale_z (p0, z0, z4))

/*
** scale_f32_z_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.s, p0/z, z4\.s
**	fscale	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (scale_f32_z_tied2, svfloat32_t, svint32_t,
		 z0_res = svscale_f32_z (p0, z4, z0),
		 z0_res = svscale_z (p0, z4, z0))

/*
** scale_f32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	fscale	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (scale_f32_z_untied, svfloat32_t, svint32_t,
	     z0 = svscale_f32_z (p0, z1, z4),
	     z0 = svscale_z (p0, z1, z4))

/*
** scale_w0_f32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	fscale	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (scale_w0_f32_z_tied1, svfloat32_t, int32_t,
		 z0 = svscale_n_f32_z (p0, z0, x0),
		 z0 = svscale_z (p0, z0, x0))

/*
** scale_w0_f32_z_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z1\.s
**	fscale	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (scale_w0_f32_z_untied, svfloat32_t, int32_t,
		 z0 = svscale_n_f32_z (p0, z1, x0),
		 z0 = svscale_z (p0, z1, x0))

/*
** scale_3_f32_z_tied1:
**	mov	(z[0-9]+\.s), #3
**	movprfx	z0\.s, p0/z, z0\.s
**	fscale	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (scale_3_f32_z_tied1, svfloat32_t,
		z0 = svscale_n_f32_z (p0, z0, 3),
		z0 = svscale_z (p0, z0, 3))

/*
** scale_3_f32_z_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.s), #3
**	movprfx	z0\.s, p0/z, z1\.s
**	fscale	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (scale_3_f32_z_untied, svfloat32_t,
		z0 = svscale_n_f32_z (p0, z1, 3),
		z0 = svscale_z (p0, z1, 3))

/*
** scale_m3_f32_z:
**	mov	(z[0-9]+\.s), #-3
**	movprfx	z0\.s, p0/z, z0\.s
**	fscale	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (scale_m3_f32_z, svfloat32_t,
		z0 = svscale_n_f32_z (p0, z0, -3),
		z0 = svscale_z (p0, z0, -3))

/*
** scale_f32_x_tied1:
**	fscale	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (scale_f32_x_tied1, svfloat32_t, svint32_t,
	     z0 = svscale_f32_x (p0, z0, z4),
	     z0 = svscale_x (p0, z0, z4))

/*
** scale_f32_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	fscale	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_DUAL_Z_REV (scale_f32_x_tied2, svfloat32_t, svint32_t,
		 z0_res = svscale_f32_x (p0, z4, z0),
		 z0_res = svscale_x (p0, z4, z0))

/*
** scale_f32_x_untied:
**	movprfx	z0, z1
**	fscale	z0\.s, p0/m, z0\.s, z4\.s
**	ret
*/
TEST_DUAL_Z (scale_f32_x_untied, svfloat32_t, svint32_t,
	     z0 = svscale_f32_x (p0, z1, z4),
	     z0 = svscale_x (p0, z1, z4))

/*
** scale_w0_f32_x_tied1:
**	mov	(z[0-9]+\.s), w0
**	fscale	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (scale_w0_f32_x_tied1, svfloat32_t, int32_t,
		 z0 = svscale_n_f32_x (p0, z0, x0),
		 z0 = svscale_x (p0, z0, x0))

/*
** scale_w0_f32_x_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	fscale	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (scale_w0_f32_x_untied, svfloat32_t, int32_t,
		 z0 = svscale_n_f32_x (p0, z1, x0),
		 z0 = svscale_x (p0, z1, x0))

/*
** scale_3_f32_x_tied1:
**	mov	(z[0-9]+\.s), #3
**	fscale	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (scale_3_f32_x_tied1, svfloat32_t,
		z0 = svscale_n_f32_x (p0, z0, 3),
		z0 = svscale_x (p0, z0, 3))

/*
** scale_3_f32_x_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.s), #3
**	movprfx	z0, z1
**	fscale	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (scale_3_f32_x_untied, svfloat32_t,
		z0 = svscale_n_f32_x (p0, z1, 3),
		z0 = svscale_x (p0, z1, 3))

/*
** scale_m3_f32_x:
**	mov	(z[0-9]+\.s), #-3
**	fscale	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (scale_m3_f32_x, svfloat32_t,
		z0 = svscale_n_f32_x (p0, z0, -3),
		z0 = svscale_x (p0, z0, -3))

/*
** ptrue_scale_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z (ptrue_scale_f32_x_tied1, svfloat32_t, svint32_t,
	     z0 = svscale_f32_x (svptrue_b32 (), z0, z4),
	     z0 = svscale_x (svptrue_b32 (), z0, z4))

/*
** ptrue_scale_f32_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z_REV (ptrue_scale_f32_x_tied2, svfloat32_t, svint32_t,
		 z0_res = svscale_f32_x (svptrue_b32 (), z4, z0),
		 z0_res = svscale_x (svptrue_b32 (), z4, z0))

/*
** ptrue_scale_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_DUAL_Z (ptrue_scale_f32_x_untied, svfloat32_t, svint32_t,
	     z0 = svscale_f32_x (svptrue_b32 (), z1, z4),
	     z0 = svscale_x (svptrue_b32 (), z1, z4))

/*
** ptrue_scale_3_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_scale_3_f32_x_tied1, svfloat32_t,
		z0 = svscale_n_f32_x (svptrue_b32 (), z0, 3),
		z0 = svscale_x (svptrue_b32 (), z0, 3))

/*
** ptrue_scale_3_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_scale_3_f32_x_untied, svfloat32_t,
		z0 = svscale_n_f32_x (svptrue_b32 (), z1, 3),
		z0 = svscale_x (svptrue_b32 (), z1, 3))

/*
** ptrue_scale_m3_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_scale_m3_f32_x_tied1, svfloat32_t,
		z0 = svscale_n_f32_x (svptrue_b32 (), z0, -3),
		z0 = svscale_x (svptrue_b32 (), z0, -3))

/*
** ptrue_scale_m3_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_scale_m3_f32_x_untied, svfloat32_t,
		z0 = svscale_n_f32_x (svptrue_b32 (), z1, -3),
		z0 = svscale_x (svptrue_b32 (), z1, -3))
