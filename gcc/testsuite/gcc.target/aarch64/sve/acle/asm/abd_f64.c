/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** abd_f64_m_tied1:
**	fabd	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (abd_f64_m_tied1, svfloat64_t,
		z0 = svabd_f64_m (p0, z0, z1),
		z0 = svabd_m (p0, z0, z1))

/*
** abd_f64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	fabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (abd_f64_m_tied2, svfloat64_t,
		z0 = svabd_f64_m (p0, z1, z0),
		z0 = svabd_m (p0, z1, z0))

/*
** abd_f64_m_untied:
**	movprfx	z0, z1
**	fabd	z0\.d, p0/m, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (abd_f64_m_untied, svfloat64_t,
		z0 = svabd_f64_m (p0, z1, z2),
		z0 = svabd_m (p0, z1, z2))

/*
** abd_d4_f64_m_tied1:
**	mov	(z[0-9]+\.d), d4
**	fabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (abd_d4_f64_m_tied1, svfloat64_t, double,
		 z0 = svabd_n_f64_m (p0, z0, d4),
		 z0 = svabd_m (p0, z0, d4))

/*
** abd_d4_f64_m_untied:
**	mov	(z[0-9]+\.d), d4
**	movprfx	z0, z1
**	fabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (abd_d4_f64_m_untied, svfloat64_t, double,
		 z0 = svabd_n_f64_m (p0, z1, d4),
		 z0 = svabd_m (p0, z1, d4))

/*
** abd_1_f64_m_tied1:
**	fmov	(z[0-9]+\.d), #1\.0(?:e\+0)?
**	fabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (abd_1_f64_m_tied1, svfloat64_t,
		z0 = svabd_n_f64_m (p0, z0, 1),
		z0 = svabd_m (p0, z0, 1))

/*
** abd_1_f64_m_untied:
**	fmov	(z[0-9]+\.d), #1\.0(?:e\+0)?
**	movprfx	z0, z1
**	fabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (abd_1_f64_m_untied, svfloat64_t,
		z0 = svabd_n_f64_m (p0, z1, 1),
		z0 = svabd_m (p0, z1, 1))

/*
** abd_f64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	fabd	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (abd_f64_z_tied1, svfloat64_t,
		z0 = svabd_f64_z (p0, z0, z1),
		z0 = svabd_z (p0, z0, z1))

/*
** abd_f64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	fabd	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (abd_f64_z_tied2, svfloat64_t,
		z0 = svabd_f64_z (p0, z1, z0),
		z0 = svabd_z (p0, z1, z0))

/*
** abd_f64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	fabd	z0\.d, p0/m, z0\.d, z2\.d
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	fabd	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (abd_f64_z_untied, svfloat64_t,
		z0 = svabd_f64_z (p0, z1, z2),
		z0 = svabd_z (p0, z1, z2))

/*
** abd_d4_f64_z_tied1:
**	mov	(z[0-9]+\.d), d4
**	movprfx	z0\.d, p0/z, z0\.d
**	fabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (abd_d4_f64_z_tied1, svfloat64_t, double,
		 z0 = svabd_n_f64_z (p0, z0, d4),
		 z0 = svabd_z (p0, z0, d4))

/*
** abd_d4_f64_z_untied:
**	mov	(z[0-9]+\.d), d4
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	fabd	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	fabd	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZD (abd_d4_f64_z_untied, svfloat64_t, double,
		 z0 = svabd_n_f64_z (p0, z1, d4),
		 z0 = svabd_z (p0, z1, d4))

/*
** abd_1_f64_z_tied1:
**	fmov	(z[0-9]+\.d), #1\.0(?:e\+0)?
**	movprfx	z0\.d, p0/z, z0\.d
**	fabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (abd_1_f64_z_tied1, svfloat64_t,
		z0 = svabd_n_f64_z (p0, z0, 1),
		z0 = svabd_z (p0, z0, 1))

/*
** abd_1_f64_z_untied:
**	fmov	(z[0-9]+\.d), #1\.0(?:e\+0)?
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	fabd	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	fabd	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (abd_1_f64_z_untied, svfloat64_t,
		z0 = svabd_n_f64_z (p0, z1, 1),
		z0 = svabd_z (p0, z1, 1))

/*
** abd_0p5_f64_z_tied1:
**	fmov	(z[0-9]+\.d), #(?:0\.5|5\.0e-1)
**	movprfx	z0\.d, p0/z, z0\.d
**	fabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (abd_0p5_f64_z_tied1, svfloat64_t,
		z0 = svabd_n_f64_z (p0, z0, 0.5),
		z0 = svabd_z (p0, z0, 0.5))

/*
** abd_0p5_f64_z_untied:
**	fmov	(z[0-9]+\.d), #(?:0\.5|5\.0e-1)
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	fabd	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	fabd	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (abd_0p5_f64_z_untied, svfloat64_t,
		z0 = svabd_n_f64_z (p0, z1, 0.5),
		z0 = svabd_z (p0, z1, 0.5))

/*
** abd_m1_f64_z_tied1:
**	fmov	(z[0-9]+\.d), #-1\.0(?:e\+0)?
**	movprfx	z0\.d, p0/z, z0\.d
**	fabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (abd_m1_f64_z_tied1, svfloat64_t,
		z0 = svabd_n_f64_z (p0, z0, -1),
		z0 = svabd_z (p0, z0, -1))

/*
** abd_m1_f64_z_untied:
**	fmov	(z[0-9]+\.d), #-1\.0(?:e\+0)?
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	fabd	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	fabd	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (abd_m1_f64_z_untied, svfloat64_t,
		z0 = svabd_n_f64_z (p0, z1, -1),
		z0 = svabd_z (p0, z1, -1))

/*
** abd_m0p5_f64_z_tied1:
**	fmov	(z[0-9]+\.d), #-(?:0\.5|5\.0e-1)
**	movprfx	z0\.d, p0/z, z0\.d
**	fabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (abd_m0p5_f64_z_tied1, svfloat64_t,
		z0 = svabd_n_f64_z (p0, z0, -0.5),
		z0 = svabd_z (p0, z0, -0.5))

/*
** abd_m0p5_f64_z_untied:
**	fmov	(z[0-9]+\.d), #-(?:0\.5|5\.0e-1)
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	fabd	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	fabd	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (abd_m0p5_f64_z_untied, svfloat64_t,
		z0 = svabd_n_f64_z (p0, z1, -0.5),
		z0 = svabd_z (p0, z1, -0.5))

/*
** abd_m2_f64_z:
**	fmov	(z[0-9]+\.d), #-2\.0(?:e\+0)?
**	movprfx	z0\.d, p0/z, z0\.d
**	fabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (abd_m2_f64_z, svfloat64_t,
		z0 = svabd_n_f64_z (p0, z0, -2),
		z0 = svabd_z (p0, z0, -2))

/*
** abd_f64_x_tied1:
**	fabd	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (abd_f64_x_tied1, svfloat64_t,
		z0 = svabd_f64_x (p0, z0, z1),
		z0 = svabd_x (p0, z0, z1))

/*
** abd_f64_x_tied2:
**	fabd	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (abd_f64_x_tied2, svfloat64_t,
		z0 = svabd_f64_x (p0, z1, z0),
		z0 = svabd_x (p0, z1, z0))

/*
** abd_f64_x_untied:
** (
**	movprfx	z0, z1
**	fabd	z0\.d, p0/m, z0\.d, z2\.d
** |
**	movprfx	z0, z2
**	fabd	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (abd_f64_x_untied, svfloat64_t,
		z0 = svabd_f64_x (p0, z1, z2),
		z0 = svabd_x (p0, z1, z2))

/*
** abd_d4_f64_x_tied1:
**	mov	(z[0-9]+\.d), d4
**	fabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (abd_d4_f64_x_tied1, svfloat64_t, double,
		 z0 = svabd_n_f64_x (p0, z0, d4),
		 z0 = svabd_x (p0, z0, d4))

/*
** abd_d4_f64_x_untied:
**	mov	z0\.d, d4
**	fabd	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_ZD (abd_d4_f64_x_untied, svfloat64_t, double,
		 z0 = svabd_n_f64_x (p0, z1, d4),
		 z0 = svabd_x (p0, z1, d4))

/*
** abd_1_f64_x_tied1:
**	fmov	(z[0-9]+\.d), #1\.0(?:e\+0)?
**	fabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (abd_1_f64_x_tied1, svfloat64_t,
		z0 = svabd_n_f64_x (p0, z0, 1),
		z0 = svabd_x (p0, z0, 1))

/*
** abd_1_f64_x_untied:
**	fmov	z0\.d, #1\.0(?:e\+0)?
**	fabd	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (abd_1_f64_x_untied, svfloat64_t,
		z0 = svabd_n_f64_x (p0, z1, 1),
		z0 = svabd_x (p0, z1, 1))

/*
** abd_0p5_f64_x_tied1:
**	fmov	(z[0-9]+\.d), #(?:0\.5|5\.0e-1)
**	fabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (abd_0p5_f64_x_tied1, svfloat64_t,
		z0 = svabd_n_f64_x (p0, z0, 0.5),
		z0 = svabd_x (p0, z0, 0.5))

/*
** abd_0p5_f64_x_untied:
**	fmov	z0\.d, #(?:0\.5|5\.0e-1)
**	fabd	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (abd_0p5_f64_x_untied, svfloat64_t,
		z0 = svabd_n_f64_x (p0, z1, 0.5),
		z0 = svabd_x (p0, z1, 0.5))

/*
** abd_m1_f64_x_tied1:
**	fmov	(z[0-9]+\.d), #-1\.0(?:e\+0)?
**	fabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (abd_m1_f64_x_tied1, svfloat64_t,
		z0 = svabd_n_f64_x (p0, z0, -1),
		z0 = svabd_x (p0, z0, -1))

/*
** abd_m1_f64_x_untied:
**	fmov	z0\.d, #-1\.0(?:e\+0)?
**	fabd	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (abd_m1_f64_x_untied, svfloat64_t,
		z0 = svabd_n_f64_x (p0, z1, -1),
		z0 = svabd_x (p0, z1, -1))

/*
** abd_m0p5_f64_x_tied1:
**	fmov	(z[0-9]+\.d), #-(?:0\.5|5\.0e-1)
**	fabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (abd_m0p5_f64_x_tied1, svfloat64_t,
		z0 = svabd_n_f64_x (p0, z0, -0.5),
		z0 = svabd_x (p0, z0, -0.5))

/*
** abd_m0p5_f64_x_untied:
**	fmov	z0\.d, #-(?:0\.5|5\.0e-1)
**	fabd	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (abd_m0p5_f64_x_untied, svfloat64_t,
		z0 = svabd_n_f64_x (p0, z1, -0.5),
		z0 = svabd_x (p0, z1, -0.5))

/*
** abd_2_f64_x_tied1:
**	fmov	(z[0-9]+\.d), #2\.0(?:e\+0)?
**	fabd	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (abd_2_f64_x_tied1, svfloat64_t,
		z0 = svabd_n_f64_x (p0, z0, 2),
		z0 = svabd_x (p0, z0, 2))

/*
** abd_2_f64_x_untied:
**	fmov	z0\.d, #2\.0(?:e\+0)?
**	fabd	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (abd_2_f64_x_untied, svfloat64_t,
		z0 = svabd_n_f64_x (p0, z1, 2),
		z0 = svabd_x (p0, z1, 2))

/*
** ptrue_abd_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_abd_f64_x_tied1, svfloat64_t,
		z0 = svabd_f64_x (svptrue_b64 (), z0, z1),
		z0 = svabd_x (svptrue_b64 (), z0, z1))

/*
** ptrue_abd_f64_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_abd_f64_x_tied2, svfloat64_t,
		z0 = svabd_f64_x (svptrue_b64 (), z1, z0),
		z0 = svabd_x (svptrue_b64 (), z1, z0))

/*
** ptrue_abd_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_abd_f64_x_untied, svfloat64_t,
		z0 = svabd_f64_x (svptrue_b64 (), z1, z2),
		z0 = svabd_x (svptrue_b64 (), z1, z2))

/*
** ptrue_abd_1_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_abd_1_f64_x_tied1, svfloat64_t,
		z0 = svabd_n_f64_x (svptrue_b64 (), z0, 1),
		z0 = svabd_x (svptrue_b64 (), z0, 1))

/*
** ptrue_abd_1_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_abd_1_f64_x_untied, svfloat64_t,
		z0 = svabd_n_f64_x (svptrue_b64 (), z1, 1),
		z0 = svabd_x (svptrue_b64 (), z1, 1))

/*
** ptrue_abd_0p5_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_abd_0p5_f64_x_tied1, svfloat64_t,
		z0 = svabd_n_f64_x (svptrue_b64 (), z0, 0.5),
		z0 = svabd_x (svptrue_b64 (), z0, 0.5))

/*
** ptrue_abd_0p5_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_abd_0p5_f64_x_untied, svfloat64_t,
		z0 = svabd_n_f64_x (svptrue_b64 (), z1, 0.5),
		z0 = svabd_x (svptrue_b64 (), z1, 0.5))

/*
** ptrue_abd_m1_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_abd_m1_f64_x_tied1, svfloat64_t,
		z0 = svabd_n_f64_x (svptrue_b64 (), z0, -1),
		z0 = svabd_x (svptrue_b64 (), z0, -1))

/*
** ptrue_abd_m1_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_abd_m1_f64_x_untied, svfloat64_t,
		z0 = svabd_n_f64_x (svptrue_b64 (), z1, -1),
		z0 = svabd_x (svptrue_b64 (), z1, -1))

/*
** ptrue_abd_m0p5_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_abd_m0p5_f64_x_tied1, svfloat64_t,
		z0 = svabd_n_f64_x (svptrue_b64 (), z0, -0.5),
		z0 = svabd_x (svptrue_b64 (), z0, -0.5))

/*
** ptrue_abd_m0p5_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_abd_m0p5_f64_x_untied, svfloat64_t,
		z0 = svabd_n_f64_x (svptrue_b64 (), z1, -0.5),
		z0 = svabd_x (svptrue_b64 (), z1, -0.5))

/*
** ptrue_abd_2_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_abd_2_f64_x_tied1, svfloat64_t,
		z0 = svabd_n_f64_x (svptrue_b64 (), z0, 2),
		z0 = svabd_x (svptrue_b64 (), z0, 2))

/*
** ptrue_abd_2_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_abd_2_f64_x_untied, svfloat64_t,
		z0 = svabd_n_f64_x (svptrue_b64 (), z1, 2),
		z0 = svabd_x (svptrue_b64 (), z1, 2))
