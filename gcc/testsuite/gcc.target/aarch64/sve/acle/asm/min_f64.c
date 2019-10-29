/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** min_f64_m_tied1:
**	fmin	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (min_f64_m_tied1, svfloat64_t,
		z0 = svmin_f64_m (p0, z0, z1),
		z0 = svmin_m (p0, z0, z1))

/*
** min_f64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	fmin	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (min_f64_m_tied2, svfloat64_t,
		z0 = svmin_f64_m (p0, z1, z0),
		z0 = svmin_m (p0, z1, z0))

/*
** min_f64_m_untied:
**	movprfx	z0, z1
**	fmin	z0\.d, p0/m, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (min_f64_m_untied, svfloat64_t,
		z0 = svmin_f64_m (p0, z1, z2),
		z0 = svmin_m (p0, z1, z2))

/*
** min_d4_f64_m_tied1:
**	mov	(z[0-9]+\.d), d4
**	fmin	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (min_d4_f64_m_tied1, svfloat64_t, double,
		 z0 = svmin_n_f64_m (p0, z0, d4),
		 z0 = svmin_m (p0, z0, d4))

/*
** min_d4_f64_m_untied:
**	mov	(z[0-9]+\.d), d4
**	movprfx	z0, z1
**	fmin	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (min_d4_f64_m_untied, svfloat64_t, double,
		 z0 = svmin_n_f64_m (p0, z1, d4),
		 z0 = svmin_m (p0, z1, d4))

/*
** min_0_f64_m_tied1:
**	fmin	z0\.d, p0/m, z0\.d, #0\.0
**	ret
*/
TEST_UNIFORM_Z (min_0_f64_m_tied1, svfloat64_t,
		z0 = svmin_n_f64_m (p0, z0, 0),
		z0 = svmin_m (p0, z0, 0))

/*
** min_0_f64_m_untied:
**	movprfx	z0, z1
**	fmin	z0\.d, p0/m, z0\.d, #0\.0
**	ret
*/
TEST_UNIFORM_Z (min_0_f64_m_untied, svfloat64_t,
		z0 = svmin_n_f64_m (p0, z1, 0),
		z0 = svmin_m (p0, z1, 0))

/*
** min_1_f64_m_tied1:
**	fmin	z0\.d, p0/m, z0\.d, #1\.0
**	ret
*/
TEST_UNIFORM_Z (min_1_f64_m_tied1, svfloat64_t,
		z0 = svmin_n_f64_m (p0, z0, 1),
		z0 = svmin_m (p0, z0, 1))

/*
** min_1_f64_m_untied:
**	movprfx	z0, z1
**	fmin	z0\.d, p0/m, z0\.d, #1\.0
**	ret
*/
TEST_UNIFORM_Z (min_1_f64_m_untied, svfloat64_t,
		z0 = svmin_n_f64_m (p0, z1, 1),
		z0 = svmin_m (p0, z1, 1))

/*
** min_2_f64_m:
**	fmov	(z[0-9]+\.d), #2\.0(?:e\+0)?
**	fmin	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (min_2_f64_m, svfloat64_t,
		z0 = svmin_n_f64_m (p0, z0, 2),
		z0 = svmin_m (p0, z0, 2))

/*
** min_f64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	fmin	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (min_f64_z_tied1, svfloat64_t,
		z0 = svmin_f64_z (p0, z0, z1),
		z0 = svmin_z (p0, z0, z1))

/*
** min_f64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	fmin	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (min_f64_z_tied2, svfloat64_t,
		z0 = svmin_f64_z (p0, z1, z0),
		z0 = svmin_z (p0, z1, z0))

/*
** min_f64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	fmin	z0\.d, p0/m, z0\.d, z2\.d
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	fmin	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (min_f64_z_untied, svfloat64_t,
		z0 = svmin_f64_z (p0, z1, z2),
		z0 = svmin_z (p0, z1, z2))

/*
** min_d4_f64_z_tied1:
**	mov	(z[0-9]+\.d), d4
**	movprfx	z0\.d, p0/z, z0\.d
**	fmin	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (min_d4_f64_z_tied1, svfloat64_t, double,
		 z0 = svmin_n_f64_z (p0, z0, d4),
		 z0 = svmin_z (p0, z0, d4))

/*
** min_d4_f64_z_untied:
**	mov	(z[0-9]+\.d), d4
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	fmin	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	fmin	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZD (min_d4_f64_z_untied, svfloat64_t, double,
		 z0 = svmin_n_f64_z (p0, z1, d4),
		 z0 = svmin_z (p0, z1, d4))

/*
** min_0_f64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	fmin	z0\.d, p0/m, z0\.d, #0\.0
**	ret
*/
TEST_UNIFORM_Z (min_0_f64_z_tied1, svfloat64_t,
		z0 = svmin_n_f64_z (p0, z0, 0),
		z0 = svmin_z (p0, z0, 0))

/*
** min_0_f64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	fmin	z0\.d, p0/m, z0\.d, #0\.0
**	ret
*/
TEST_UNIFORM_Z (min_0_f64_z_untied, svfloat64_t,
		z0 = svmin_n_f64_z (p0, z1, 0),
		z0 = svmin_z (p0, z1, 0))

/*
** min_1_f64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	fmin	z0\.d, p0/m, z0\.d, #1\.0
**	ret
*/
TEST_UNIFORM_Z (min_1_f64_z_tied1, svfloat64_t,
		z0 = svmin_n_f64_z (p0, z0, 1),
		z0 = svmin_z (p0, z0, 1))

/*
** min_1_f64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	fmin	z0\.d, p0/m, z0\.d, #1\.0
**	ret
*/
TEST_UNIFORM_Z (min_1_f64_z_untied, svfloat64_t,
		z0 = svmin_n_f64_z (p0, z1, 1),
		z0 = svmin_z (p0, z1, 1))

/*
** min_2_f64_z:
**	fmov	(z[0-9]+\.d), #2\.0(?:e\+0)?
**	movprfx	z0\.d, p0/z, z0\.d
**	fmin	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (min_2_f64_z, svfloat64_t,
		z0 = svmin_n_f64_z (p0, z0, 2),
		z0 = svmin_z (p0, z0, 2))

/*
** min_f64_x_tied1:
**	fmin	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (min_f64_x_tied1, svfloat64_t,
		z0 = svmin_f64_x (p0, z0, z1),
		z0 = svmin_x (p0, z0, z1))

/*
** min_f64_x_tied2:
**	fmin	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (min_f64_x_tied2, svfloat64_t,
		z0 = svmin_f64_x (p0, z1, z0),
		z0 = svmin_x (p0, z1, z0))

/*
** min_f64_x_untied:
** (
**	movprfx	z0, z1
**	fmin	z0\.d, p0/m, z0\.d, z2\.d
** |
**	movprfx	z0, z2
**	fmin	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (min_f64_x_untied, svfloat64_t,
		z0 = svmin_f64_x (p0, z1, z2),
		z0 = svmin_x (p0, z1, z2))

/*
** min_d4_f64_x_tied1:
**	mov	(z[0-9]+\.d), d4
**	fmin	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (min_d4_f64_x_tied1, svfloat64_t, double,
		 z0 = svmin_n_f64_x (p0, z0, d4),
		 z0 = svmin_x (p0, z0, d4))

/*
** min_d4_f64_x_untied:
**	mov	z0\.d, d4
**	fmin	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_ZD (min_d4_f64_x_untied, svfloat64_t, double,
		 z0 = svmin_n_f64_x (p0, z1, d4),
		 z0 = svmin_x (p0, z1, d4))

/*
** min_0_f64_x_tied1:
**	fmin	z0\.d, p0/m, z0\.d, #0\.0
**	ret
*/
TEST_UNIFORM_Z (min_0_f64_x_tied1, svfloat64_t,
		z0 = svmin_n_f64_x (p0, z0, 0),
		z0 = svmin_x (p0, z0, 0))

/*
** min_0_f64_x_untied:
**	movprfx	z0, z1
**	fmin	z0\.d, p0/m, z0\.d, #0\.0
**	ret
*/
TEST_UNIFORM_Z (min_0_f64_x_untied, svfloat64_t,
		z0 = svmin_n_f64_x (p0, z1, 0),
		z0 = svmin_x (p0, z1, 0))

/*
** min_1_f64_x_tied1:
**	fmin	z0\.d, p0/m, z0\.d, #1\.0
**	ret
*/
TEST_UNIFORM_Z (min_1_f64_x_tied1, svfloat64_t,
		z0 = svmin_n_f64_x (p0, z0, 1),
		z0 = svmin_x (p0, z0, 1))

/*
** min_1_f64_x_untied:
**	movprfx	z0, z1
**	fmin	z0\.d, p0/m, z0\.d, #1\.0
**	ret
*/
TEST_UNIFORM_Z (min_1_f64_x_untied, svfloat64_t,
		z0 = svmin_n_f64_x (p0, z1, 1),
		z0 = svmin_x (p0, z1, 1))

/*
** min_2_f64_x_tied1:
**	fmov	(z[0-9]+\.d), #2\.0(?:e\+0)?
**	fmin	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (min_2_f64_x_tied1, svfloat64_t,
		z0 = svmin_n_f64_x (p0, z0, 2),
		z0 = svmin_x (p0, z0, 2))

/*
** min_2_f64_x_untied:
**	fmov	z0\.d, #2\.0(?:e\+0)?
**	fmin	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (min_2_f64_x_untied, svfloat64_t,
		z0 = svmin_n_f64_x (p0, z1, 2),
		z0 = svmin_x (p0, z1, 2))

/*
** ptrue_min_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_min_f64_x_tied1, svfloat64_t,
		z0 = svmin_f64_x (svptrue_b64 (), z0, z1),
		z0 = svmin_x (svptrue_b64 (), z0, z1))

/*
** ptrue_min_f64_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_min_f64_x_tied2, svfloat64_t,
		z0 = svmin_f64_x (svptrue_b64 (), z1, z0),
		z0 = svmin_x (svptrue_b64 (), z1, z0))

/*
** ptrue_min_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_min_f64_x_untied, svfloat64_t,
		z0 = svmin_f64_x (svptrue_b64 (), z1, z2),
		z0 = svmin_x (svptrue_b64 (), z1, z2))

/*
** ptrue_min_0_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_min_0_f64_x_tied1, svfloat64_t,
		z0 = svmin_n_f64_x (svptrue_b64 (), z0, 0),
		z0 = svmin_x (svptrue_b64 (), z0, 0))

/*
** ptrue_min_0_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_min_0_f64_x_untied, svfloat64_t,
		z0 = svmin_n_f64_x (svptrue_b64 (), z1, 0),
		z0 = svmin_x (svptrue_b64 (), z1, 0))

/*
** ptrue_min_1_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_min_1_f64_x_tied1, svfloat64_t,
		z0 = svmin_n_f64_x (svptrue_b64 (), z0, 1),
		z0 = svmin_x (svptrue_b64 (), z0, 1))

/*
** ptrue_min_1_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_min_1_f64_x_untied, svfloat64_t,
		z0 = svmin_n_f64_x (svptrue_b64 (), z1, 1),
		z0 = svmin_x (svptrue_b64 (), z1, 1))

/*
** ptrue_min_2_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_min_2_f64_x_tied1, svfloat64_t,
		z0 = svmin_n_f64_x (svptrue_b64 (), z0, 2),
		z0 = svmin_x (svptrue_b64 (), z0, 2))

/*
** ptrue_min_2_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_min_2_f64_x_untied, svfloat64_t,
		z0 = svmin_n_f64_x (svptrue_b64 (), z1, 2),
		z0 = svmin_x (svptrue_b64 (), z1, 2))
