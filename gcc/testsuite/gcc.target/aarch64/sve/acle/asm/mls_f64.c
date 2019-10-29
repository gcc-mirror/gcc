/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mls_f64_m_tied1:
**	fmls	z0\.d, p0/m, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (mls_f64_m_tied1, svfloat64_t,
		z0 = svmls_f64_m (p0, z0, z1, z2),
		z0 = svmls_m (p0, z0, z1, z2))

/*
** mls_f64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	fmls	z0\.d, p0/m, \1, z2\.d
**	ret
*/
TEST_UNIFORM_Z (mls_f64_m_tied2, svfloat64_t,
		z0 = svmls_f64_m (p0, z1, z0, z2),
		z0 = svmls_m (p0, z1, z0, z2))

/*
** mls_f64_m_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	fmls	z0\.d, p0/m, z2\.d, \1
**	ret
*/
TEST_UNIFORM_Z (mls_f64_m_tied3, svfloat64_t,
		z0 = svmls_f64_m (p0, z1, z2, z0),
		z0 = svmls_m (p0, z1, z2, z0))

/*
** mls_f64_m_untied:
**	movprfx	z0, z1
**	fmls	z0\.d, p0/m, z2\.d, z3\.d
**	ret
*/
TEST_UNIFORM_Z (mls_f64_m_untied, svfloat64_t,
		z0 = svmls_f64_m (p0, z1, z2, z3),
		z0 = svmls_m (p0, z1, z2, z3))

/*
** mls_d4_f64_m_tied1:
**	mov	(z[0-9]+\.d), d4
**	fmls	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (mls_d4_f64_m_tied1, svfloat64_t, double,
		 z0 = svmls_n_f64_m (p0, z0, z1, d4),
		 z0 = svmls_m (p0, z0, z1, d4))

/*
** mls_d4_f64_m_untied:
**	mov	(z[0-9]+\.d), d4
**	movprfx	z0, z1
**	fmls	z0\.d, p0/m, z2\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (mls_d4_f64_m_untied, svfloat64_t, double,
		 z0 = svmls_n_f64_m (p0, z1, z2, d4),
		 z0 = svmls_m (p0, z1, z2, d4))

/*
** mls_2_f64_m_tied1:
**	fmov	(z[0-9]+\.d), #2\.0(?:e\+0)?
**	fmls	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (mls_2_f64_m_tied1, svfloat64_t,
		z0 = svmls_n_f64_m (p0, z0, z1, 2),
		z0 = svmls_m (p0, z0, z1, 2))

/*
** mls_2_f64_m_untied: { xfail *-*-* }
**	fmov	(z[0-9]+\.d), #2\.0(?:e\+0)?
**	movprfx	z0, z1
**	fmls	z0\.d, p0/m, z2\.d, \1
**	ret
*/
TEST_UNIFORM_Z (mls_2_f64_m_untied, svfloat64_t,
		z0 = svmls_n_f64_m (p0, z1, z2, 2),
		z0 = svmls_m (p0, z1, z2, 2))

/*
** mls_f64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	fmls	z0\.d, p0/m, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (mls_f64_z_tied1, svfloat64_t,
		z0 = svmls_f64_z (p0, z0, z1, z2),
		z0 = svmls_z (p0, z0, z1, z2))

/*
** mls_f64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	fmsb	z0\.d, p0/m, z2\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mls_f64_z_tied2, svfloat64_t,
		z0 = svmls_f64_z (p0, z1, z0, z2),
		z0 = svmls_z (p0, z1, z0, z2))

/*
** mls_f64_z_tied3:
**	movprfx	z0\.d, p0/z, z0\.d
**	fmsb	z0\.d, p0/m, z2\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mls_f64_z_tied3, svfloat64_t,
		z0 = svmls_f64_z (p0, z1, z2, z0),
		z0 = svmls_z (p0, z1, z2, z0))

/*
** mls_f64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	fmls	z0\.d, p0/m, z2\.d, z3\.d
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	fmsb	z0\.d, p0/m, z3\.d, z1\.d
** |
**	movprfx	z0\.d, p0/z, z3\.d
**	fmsb	z0\.d, p0/m, z2\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (mls_f64_z_untied, svfloat64_t,
		z0 = svmls_f64_z (p0, z1, z2, z3),
		z0 = svmls_z (p0, z1, z2, z3))

/*
** mls_d4_f64_z_tied1:
**	mov	(z[0-9]+\.d), d4
**	movprfx	z0\.d, p0/z, z0\.d
**	fmls	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (mls_d4_f64_z_tied1, svfloat64_t, double,
		 z0 = svmls_n_f64_z (p0, z0, z1, d4),
		 z0 = svmls_z (p0, z0, z1, d4))

/*
** mls_d4_f64_z_tied2:
**	mov	(z[0-9]+\.d), d4
**	movprfx	z0\.d, p0/z, z0\.d
**	fmsb	z0\.d, p0/m, \1, z1\.d
**	ret
*/
TEST_UNIFORM_ZD (mls_d4_f64_z_tied2, svfloat64_t, double,
		 z0 = svmls_n_f64_z (p0, z1, z0, d4),
		 z0 = svmls_z (p0, z1, z0, d4))

/*
** mls_d4_f64_z_untied:
**	mov	(z[0-9]+\.d), d4
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	fmls	z0\.d, p0/m, z2\.d, \1
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	fmsb	z0\.d, p0/m, \1, z1\.d
** |
**	movprfx	z0\.d, p0/z, \1
**	fmsb	z0\.d, p0/m, z2\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZD (mls_d4_f64_z_untied, svfloat64_t, double,
		 z0 = svmls_n_f64_z (p0, z1, z2, d4),
		 z0 = svmls_z (p0, z1, z2, d4))

/*
** mls_2_f64_z_tied1:
**	fmov	(z[0-9]+\.d), #2\.0(?:e\+0)?
**	movprfx	z0\.d, p0/z, z0\.d
**	fmls	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (mls_2_f64_z_tied1, svfloat64_t,
		z0 = svmls_n_f64_z (p0, z0, z1, 2),
		z0 = svmls_z (p0, z0, z1, 2))

/*
** mls_2_f64_z_tied2:
**	fmov	(z[0-9]+\.d), #2\.0(?:e\+0)?
**	movprfx	z0\.d, p0/z, z0\.d
**	fmsb	z0\.d, p0/m, \1, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mls_2_f64_z_tied2, svfloat64_t,
		z0 = svmls_n_f64_z (p0, z1, z0, 2),
		z0 = svmls_z (p0, z1, z0, 2))

/*
** mls_2_f64_z_untied:
**	fmov	(z[0-9]+\.d), #2\.0(?:e\+0)?
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	fmls	z0\.d, p0/m, z2\.d, \1
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	fmsb	z0\.d, p0/m, \1, z1\.d
** |
**	movprfx	z0\.d, p0/z, \1
**	fmsb	z0\.d, p0/m, z2\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (mls_2_f64_z_untied, svfloat64_t,
		z0 = svmls_n_f64_z (p0, z1, z2, 2),
		z0 = svmls_z (p0, z1, z2, 2))

/*
** mls_f64_x_tied1:
**	fmls	z0\.d, p0/m, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (mls_f64_x_tied1, svfloat64_t,
		z0 = svmls_f64_x (p0, z0, z1, z2),
		z0 = svmls_x (p0, z0, z1, z2))

/*
** mls_f64_x_tied2:
**	fmsb	z0\.d, p0/m, z2\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mls_f64_x_tied2, svfloat64_t,
		z0 = svmls_f64_x (p0, z1, z0, z2),
		z0 = svmls_x (p0, z1, z0, z2))

/*
** mls_f64_x_tied3:
**	fmsb	z0\.d, p0/m, z2\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mls_f64_x_tied3, svfloat64_t,
		z0 = svmls_f64_x (p0, z1, z2, z0),
		z0 = svmls_x (p0, z1, z2, z0))

/*
** mls_f64_x_untied:
** (
**	movprfx	z0, z1
**	fmls	z0\.d, p0/m, z2\.d, z3\.d
** |
**	movprfx	z0, z2
**	fmsb	z0\.d, p0/m, z3\.d, z1\.d
** |
**	movprfx	z0, z3
**	fmsb	z0\.d, p0/m, z2\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (mls_f64_x_untied, svfloat64_t,
		z0 = svmls_f64_x (p0, z1, z2, z3),
		z0 = svmls_x (p0, z1, z2, z3))

/*
** mls_d4_f64_x_tied1:
**	mov	(z[0-9]+\.d), d4
**	fmls	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (mls_d4_f64_x_tied1, svfloat64_t, double,
		 z0 = svmls_n_f64_x (p0, z0, z1, d4),
		 z0 = svmls_x (p0, z0, z1, d4))

/*
** mls_d4_f64_x_tied2:
**	mov	(z[0-9]+\.d), d4
**	fmsb	z0\.d, p0/m, \1, z1\.d
**	ret
*/
TEST_UNIFORM_ZD (mls_d4_f64_x_tied2, svfloat64_t, double,
		 z0 = svmls_n_f64_x (p0, z1, z0, d4),
		 z0 = svmls_x (p0, z1, z0, d4))

/*
** mls_d4_f64_x_untied: { xfail *-*-* }
**	mov	z0\.d, d4
**	fmsb	z0\.d, p0/m, z2\.d, z1\.d
**	ret
*/
TEST_UNIFORM_ZD (mls_d4_f64_x_untied, svfloat64_t, double,
		 z0 = svmls_n_f64_x (p0, z1, z2, d4),
		 z0 = svmls_x (p0, z1, z2, d4))

/*
** mls_2_f64_x_tied1:
**	fmov	(z[0-9]+\.d), #2\.0(?:e\+0)?
**	fmls	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (mls_2_f64_x_tied1, svfloat64_t,
		z0 = svmls_n_f64_x (p0, z0, z1, 2),
		z0 = svmls_x (p0, z0, z1, 2))

/*
** mls_2_f64_x_tied2:
**	fmov	(z[0-9]+\.d), #2\.0(?:e\+0)?
**	fmsb	z0\.d, p0/m, \1, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mls_2_f64_x_tied2, svfloat64_t,
		z0 = svmls_n_f64_x (p0, z1, z0, 2),
		z0 = svmls_x (p0, z1, z0, 2))

/*
** mls_2_f64_x_untied:
**	fmov	z0\.d, #2\.0(?:e\+0)?
**	fmsb	z0\.d, p0/m, z2\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mls_2_f64_x_untied, svfloat64_t,
		z0 = svmls_n_f64_x (p0, z1, z2, 2),
		z0 = svmls_x (p0, z1, z2, 2))

/*
** ptrue_mls_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_mls_f64_x_tied1, svfloat64_t,
		z0 = svmls_f64_x (svptrue_b64 (), z0, z1, z2),
		z0 = svmls_x (svptrue_b64 (), z0, z1, z2))

/*
** ptrue_mls_f64_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_mls_f64_x_tied2, svfloat64_t,
		z0 = svmls_f64_x (svptrue_b64 (), z1, z0, z2),
		z0 = svmls_x (svptrue_b64 (), z1, z0, z2))

/*
** ptrue_mls_f64_x_tied3:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_mls_f64_x_tied3, svfloat64_t,
		z0 = svmls_f64_x (svptrue_b64 (), z1, z2, z0),
		z0 = svmls_x (svptrue_b64 (), z1, z2, z0))

/*
** ptrue_mls_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_mls_f64_x_untied, svfloat64_t,
		z0 = svmls_f64_x (svptrue_b64 (), z1, z2, z3),
		z0 = svmls_x (svptrue_b64 (), z1, z2, z3))

/*
** ptrue_mls_2_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_mls_2_f64_x_tied1, svfloat64_t,
		z0 = svmls_n_f64_x (svptrue_b64 (), z0, z1, 2),
		z0 = svmls_x (svptrue_b64 (), z0, z1, 2))

/*
** ptrue_mls_2_f64_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_mls_2_f64_x_tied2, svfloat64_t,
		z0 = svmls_n_f64_x (svptrue_b64 (), z1, z0, 2),
		z0 = svmls_x (svptrue_b64 (), z1, z0, 2))

/*
** ptrue_mls_2_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_mls_2_f64_x_untied, svfloat64_t,
		z0 = svmls_n_f64_x (svptrue_b64 (), z1, z2, 2),
		z0 = svmls_x (svptrue_b64 (), z1, z2, 2))
