/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** div_f64_m_tied1:
**	fdiv	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (div_f64_m_tied1, svfloat64_t,
		z0 = svdiv_f64_m (p0, z0, z1),
		z0 = svdiv_m (p0, z0, z1))

/*
** div_f64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	fdiv	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (div_f64_m_tied2, svfloat64_t,
		z0 = svdiv_f64_m (p0, z1, z0),
		z0 = svdiv_m (p0, z1, z0))

/*
** div_f64_m_untied:
**	movprfx	z0, z1
**	fdiv	z0\.d, p0/m, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (div_f64_m_untied, svfloat64_t,
		z0 = svdiv_f64_m (p0, z1, z2),
		z0 = svdiv_m (p0, z1, z2))

/*
** div_d4_f64_m_tied1:
**	mov	(z[0-9]+\.d), d4
**	fdiv	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (div_d4_f64_m_tied1, svfloat64_t, double,
		 z0 = svdiv_n_f64_m (p0, z0, d4),
		 z0 = svdiv_m (p0, z0, d4))

/*
** div_d4_f64_m_untied:
**	mov	(z[0-9]+\.d), d4
**	movprfx	z0, z1
**	fdiv	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (div_d4_f64_m_untied, svfloat64_t, double,
		 z0 = svdiv_n_f64_m (p0, z1, d4),
		 z0 = svdiv_m (p0, z1, d4))

/*
** div_1_f64_m_tied1:
**	fmov	(z[0-9]+\.d), #1\.0(?:e\+0)?
**	fdiv	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (div_1_f64_m_tied1, svfloat64_t,
		z0 = svdiv_n_f64_m (p0, z0, 1),
		z0 = svdiv_m (p0, z0, 1))

/*
** div_1_f64_m_untied: { xfail *-*-* }
**	fmov	(z[0-9]+\.d), #1\.0(?:e\+0)?
**	movprfx	z0, z1
**	fdiv	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (div_1_f64_m_untied, svfloat64_t,
		z0 = svdiv_n_f64_m (p0, z1, 1),
		z0 = svdiv_m (p0, z1, 1))

/*
** div_f64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	fdiv	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (div_f64_z_tied1, svfloat64_t,
		z0 = svdiv_f64_z (p0, z0, z1),
		z0 = svdiv_z (p0, z0, z1))

/*
** div_f64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	fdivr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (div_f64_z_tied2, svfloat64_t,
		z0 = svdiv_f64_z (p0, z1, z0),
		z0 = svdiv_z (p0, z1, z0))

/*
** div_f64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	fdiv	z0\.d, p0/m, z0\.d, z2\.d
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	fdivr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (div_f64_z_untied, svfloat64_t,
		z0 = svdiv_f64_z (p0, z1, z2),
		z0 = svdiv_z (p0, z1, z2))

/*
** div_d4_f64_z_tied1:
**	mov	(z[0-9]+\.d), d4
**	movprfx	z0\.d, p0/z, z0\.d
**	fdiv	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (div_d4_f64_z_tied1, svfloat64_t, double,
		 z0 = svdiv_n_f64_z (p0, z0, d4),
		 z0 = svdiv_z (p0, z0, d4))

/*
** div_d4_f64_z_untied:
**	mov	(z[0-9]+\.d), d4
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	fdiv	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	fdivr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZD (div_d4_f64_z_untied, svfloat64_t, double,
		 z0 = svdiv_n_f64_z (p0, z1, d4),
		 z0 = svdiv_z (p0, z1, d4))

/*
** div_1_f64_z_tied1:
**	fmov	(z[0-9]+\.d), #1\.0(?:e\+0)?
**	movprfx	z0\.d, p0/z, z0\.d
**	fdiv	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (div_1_f64_z_tied1, svfloat64_t,
		z0 = svdiv_n_f64_z (p0, z0, 1),
		z0 = svdiv_z (p0, z0, 1))

/*
** div_1_f64_z_untied:
**	fmov	(z[0-9]+\.d), #1\.0(?:e\+0)?
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	fdiv	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	fdivr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (div_1_f64_z_untied, svfloat64_t,
		z0 = svdiv_n_f64_z (p0, z1, 1),
		z0 = svdiv_z (p0, z1, 1))

/*
** div_0p5_f64_z:
**	fmov	(z[0-9]+\.d), #(?:0\.5|5\.0e-1)
**	movprfx	z0\.d, p0/z, z0\.d
**	fdiv	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (div_0p5_f64_z, svfloat64_t,
		z0 = svdiv_n_f64_z (p0, z0, 0.5),
		z0 = svdiv_z (p0, z0, 0.5))

/*
** div_f64_x_tied1:
**	fdiv	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (div_f64_x_tied1, svfloat64_t,
		z0 = svdiv_f64_x (p0, z0, z1),
		z0 = svdiv_x (p0, z0, z1))

/*
** div_f64_x_tied2:
**	fdivr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (div_f64_x_tied2, svfloat64_t,
		z0 = svdiv_f64_x (p0, z1, z0),
		z0 = svdiv_x (p0, z1, z0))

/*
** div_f64_x_untied:
** (
**	movprfx	z0, z1
**	fdiv	z0\.d, p0/m, z0\.d, z2\.d
** |
**	movprfx	z0, z2
**	fdivr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (div_f64_x_untied, svfloat64_t,
		z0 = svdiv_f64_x (p0, z1, z2),
		z0 = svdiv_x (p0, z1, z2))

/*
** div_d4_f64_x_tied1:
**	mov	(z[0-9]+\.d), d4
**	fdiv	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (div_d4_f64_x_tied1, svfloat64_t, double,
		 z0 = svdiv_n_f64_x (p0, z0, d4),
		 z0 = svdiv_x (p0, z0, d4))

/*
** div_d4_f64_x_untied: { xfail *-*-* }
**	mov	z0\.d, d4
**	fdivr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_ZD (div_d4_f64_x_untied, svfloat64_t, double,
		 z0 = svdiv_n_f64_x (p0, z1, d4),
		 z0 = svdiv_x (p0, z1, d4))

/*
** div_1_f64_x_tied1:
**	fmov	(z[0-9]+\.d), #1\.0(?:e\+0)?
**	fdiv	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (div_1_f64_x_tied1, svfloat64_t,
		z0 = svdiv_n_f64_x (p0, z0, 1),
		z0 = svdiv_x (p0, z0, 1))

/*
** div_1_f64_x_untied:
**	fmov	z0\.d, #1\.0(?:e\+0)?
**	fdivr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (div_1_f64_x_untied, svfloat64_t,
		z0 = svdiv_n_f64_x (p0, z1, 1),
		z0 = svdiv_x (p0, z1, 1))

/*
** ptrue_div_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_div_f64_x_tied1, svfloat64_t,
		z0 = svdiv_f64_x (svptrue_b64 (), z0, z1),
		z0 = svdiv_x (svptrue_b64 (), z0, z1))

/*
** ptrue_div_f64_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_div_f64_x_tied2, svfloat64_t,
		z0 = svdiv_f64_x (svptrue_b64 (), z1, z0),
		z0 = svdiv_x (svptrue_b64 (), z1, z0))

/*
** ptrue_div_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_div_f64_x_untied, svfloat64_t,
		z0 = svdiv_f64_x (svptrue_b64 (), z1, z2),
		z0 = svdiv_x (svptrue_b64 (), z1, z2))

/*
** ptrue_div_1_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_div_1_f64_x_tied1, svfloat64_t,
		z0 = svdiv_n_f64_x (svptrue_b64 (), z0, 1),
		z0 = svdiv_x (svptrue_b64 (), z0, 1))

/*
** ptrue_div_1_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_div_1_f64_x_untied, svfloat64_t,
		z0 = svdiv_n_f64_x (svptrue_b64 (), z1, 1),
		z0 = svdiv_x (svptrue_b64 (), z1, 1))
