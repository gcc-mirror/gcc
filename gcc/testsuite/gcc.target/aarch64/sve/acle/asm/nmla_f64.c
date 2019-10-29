/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** nmla_f64_m_tied1:
**	fnmla	z0\.d, p0/m, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (nmla_f64_m_tied1, svfloat64_t,
		z0 = svnmla_f64_m (p0, z0, z1, z2),
		z0 = svnmla_m (p0, z0, z1, z2))

/*
** nmla_f64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	fnmla	z0\.d, p0/m, \1, z2\.d
**	ret
*/
TEST_UNIFORM_Z (nmla_f64_m_tied2, svfloat64_t,
		z0 = svnmla_f64_m (p0, z1, z0, z2),
		z0 = svnmla_m (p0, z1, z0, z2))

/*
** nmla_f64_m_tied3:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	fnmla	z0\.d, p0/m, z2\.d, \1
**	ret
*/
TEST_UNIFORM_Z (nmla_f64_m_tied3, svfloat64_t,
		z0 = svnmla_f64_m (p0, z1, z2, z0),
		z0 = svnmla_m (p0, z1, z2, z0))

/*
** nmla_f64_m_untied:
**	movprfx	z0, z1
**	fnmla	z0\.d, p0/m, z2\.d, z3\.d
**	ret
*/
TEST_UNIFORM_Z (nmla_f64_m_untied, svfloat64_t,
		z0 = svnmla_f64_m (p0, z1, z2, z3),
		z0 = svnmla_m (p0, z1, z2, z3))

/*
** nmla_d4_f64_m_tied1:
**	mov	(z[0-9]+\.d), d4
**	fnmla	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (nmla_d4_f64_m_tied1, svfloat64_t, double,
		 z0 = svnmla_n_f64_m (p0, z0, z1, d4),
		 z0 = svnmla_m (p0, z0, z1, d4))

/*
** nmla_d4_f64_m_untied:
**	mov	(z[0-9]+\.d), d4
**	movprfx	z0, z1
**	fnmla	z0\.d, p0/m, z2\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (nmla_d4_f64_m_untied, svfloat64_t, double,
		 z0 = svnmla_n_f64_m (p0, z1, z2, d4),
		 z0 = svnmla_m (p0, z1, z2, d4))

/*
** nmla_2_f64_m_tied1:
**	fmov	(z[0-9]+\.d), #2\.0(?:e\+0)?
**	fnmla	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (nmla_2_f64_m_tied1, svfloat64_t,
		z0 = svnmla_n_f64_m (p0, z0, z1, 2),
		z0 = svnmla_m (p0, z0, z1, 2))

/*
** nmla_2_f64_m_untied: { xfail *-*-* }
**	fmov	(z[0-9]+\.d), #2\.0(?:e\+0)?
**	movprfx	z0, z1
**	fnmla	z0\.d, p0/m, z2\.d, \1
**	ret
*/
TEST_UNIFORM_Z (nmla_2_f64_m_untied, svfloat64_t,
		z0 = svnmla_n_f64_m (p0, z1, z2, 2),
		z0 = svnmla_m (p0, z1, z2, 2))

/*
** nmla_f64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	fnmla	z0\.d, p0/m, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (nmla_f64_z_tied1, svfloat64_t,
		z0 = svnmla_f64_z (p0, z0, z1, z2),
		z0 = svnmla_z (p0, z0, z1, z2))

/*
** nmla_f64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	fnmad	z0\.d, p0/m, z2\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (nmla_f64_z_tied2, svfloat64_t,
		z0 = svnmla_f64_z (p0, z1, z0, z2),
		z0 = svnmla_z (p0, z1, z0, z2))

/*
** nmla_f64_z_tied3:
**	movprfx	z0\.d, p0/z, z0\.d
**	fnmad	z0\.d, p0/m, z2\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (nmla_f64_z_tied3, svfloat64_t,
		z0 = svnmla_f64_z (p0, z1, z2, z0),
		z0 = svnmla_z (p0, z1, z2, z0))

/*
** nmla_f64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	fnmla	z0\.d, p0/m, z2\.d, z3\.d
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	fnmad	z0\.d, p0/m, z3\.d, z1\.d
** |
**	movprfx	z0\.d, p0/z, z3\.d
**	fnmad	z0\.d, p0/m, z2\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (nmla_f64_z_untied, svfloat64_t,
		z0 = svnmla_f64_z (p0, z1, z2, z3),
		z0 = svnmla_z (p0, z1, z2, z3))

/*
** nmla_d4_f64_z_tied1:
**	mov	(z[0-9]+\.d), d4
**	movprfx	z0\.d, p0/z, z0\.d
**	fnmla	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (nmla_d4_f64_z_tied1, svfloat64_t, double,
		 z0 = svnmla_n_f64_z (p0, z0, z1, d4),
		 z0 = svnmla_z (p0, z0, z1, d4))

/*
** nmla_d4_f64_z_tied2:
**	mov	(z[0-9]+\.d), d4
**	movprfx	z0\.d, p0/z, z0\.d
**	fnmad	z0\.d, p0/m, \1, z1\.d
**	ret
*/
TEST_UNIFORM_ZD (nmla_d4_f64_z_tied2, svfloat64_t, double,
		 z0 = svnmla_n_f64_z (p0, z1, z0, d4),
		 z0 = svnmla_z (p0, z1, z0, d4))

/*
** nmla_d4_f64_z_untied:
**	mov	(z[0-9]+\.d), d4
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	fnmla	z0\.d, p0/m, z2\.d, \1
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	fnmad	z0\.d, p0/m, \1, z1\.d
** |
**	movprfx	z0\.d, p0/z, \1
**	fnmad	z0\.d, p0/m, z2\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZD (nmla_d4_f64_z_untied, svfloat64_t, double,
		 z0 = svnmla_n_f64_z (p0, z1, z2, d4),
		 z0 = svnmla_z (p0, z1, z2, d4))

/*
** nmla_2_f64_z_tied1:
**	fmov	(z[0-9]+\.d), #2\.0(?:e\+0)?
**	movprfx	z0\.d, p0/z, z0\.d
**	fnmla	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (nmla_2_f64_z_tied1, svfloat64_t,
		z0 = svnmla_n_f64_z (p0, z0, z1, 2),
		z0 = svnmla_z (p0, z0, z1, 2))

/*
** nmla_2_f64_z_tied2:
**	fmov	(z[0-9]+\.d), #2\.0(?:e\+0)?
**	movprfx	z0\.d, p0/z, z0\.d
**	fnmad	z0\.d, p0/m, \1, z1\.d
**	ret
*/
TEST_UNIFORM_Z (nmla_2_f64_z_tied2, svfloat64_t,
		z0 = svnmla_n_f64_z (p0, z1, z0, 2),
		z0 = svnmla_z (p0, z1, z0, 2))

/*
** nmla_2_f64_z_untied:
**	fmov	(z[0-9]+\.d), #2\.0(?:e\+0)?
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	fnmla	z0\.d, p0/m, z2\.d, \1
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	fnmad	z0\.d, p0/m, \1, z1\.d
** |
**	movprfx	z0\.d, p0/z, \1
**	fnmad	z0\.d, p0/m, z2\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (nmla_2_f64_z_untied, svfloat64_t,
		z0 = svnmla_n_f64_z (p0, z1, z2, 2),
		z0 = svnmla_z (p0, z1, z2, 2))

/*
** nmla_f64_x_tied1:
**	fnmla	z0\.d, p0/m, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (nmla_f64_x_tied1, svfloat64_t,
		z0 = svnmla_f64_x (p0, z0, z1, z2),
		z0 = svnmla_x (p0, z0, z1, z2))

/*
** nmla_f64_x_tied2:
**	fnmad	z0\.d, p0/m, z2\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (nmla_f64_x_tied2, svfloat64_t,
		z0 = svnmla_f64_x (p0, z1, z0, z2),
		z0 = svnmla_x (p0, z1, z0, z2))

/*
** nmla_f64_x_tied3:
**	fnmad	z0\.d, p0/m, z2\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (nmla_f64_x_tied3, svfloat64_t,
		z0 = svnmla_f64_x (p0, z1, z2, z0),
		z0 = svnmla_x (p0, z1, z2, z0))

/*
** nmla_f64_x_untied:
** (
**	movprfx	z0, z1
**	fnmla	z0\.d, p0/m, z2\.d, z3\.d
** |
**	movprfx	z0, z2
**	fnmad	z0\.d, p0/m, z3\.d, z1\.d
** |
**	movprfx	z0, z3
**	fnmad	z0\.d, p0/m, z2\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (nmla_f64_x_untied, svfloat64_t,
		z0 = svnmla_f64_x (p0, z1, z2, z3),
		z0 = svnmla_x (p0, z1, z2, z3))

/*
** nmla_d4_f64_x_tied1:
**	mov	(z[0-9]+\.d), d4
**	fnmla	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (nmla_d4_f64_x_tied1, svfloat64_t, double,
		 z0 = svnmla_n_f64_x (p0, z0, z1, d4),
		 z0 = svnmla_x (p0, z0, z1, d4))

/*
** nmla_d4_f64_x_tied2:
**	mov	(z[0-9]+\.d), d4
**	fnmad	z0\.d, p0/m, \1, z1\.d
**	ret
*/
TEST_UNIFORM_ZD (nmla_d4_f64_x_tied2, svfloat64_t, double,
		 z0 = svnmla_n_f64_x (p0, z1, z0, d4),
		 z0 = svnmla_x (p0, z1, z0, d4))

/*
** nmla_d4_f64_x_untied: { xfail *-*-* }
**	mov	z0\.d, d4
**	fnmad	z0\.d, p0/m, z2\.d, z1\.d
**	ret
*/
TEST_UNIFORM_ZD (nmla_d4_f64_x_untied, svfloat64_t, double,
		 z0 = svnmla_n_f64_x (p0, z1, z2, d4),
		 z0 = svnmla_x (p0, z1, z2, d4))

/*
** nmla_2_f64_x_tied1:
**	fmov	(z[0-9]+\.d), #2\.0(?:e\+0)?
**	fnmla	z0\.d, p0/m, z1\.d, \1
**	ret
*/
TEST_UNIFORM_Z (nmla_2_f64_x_tied1, svfloat64_t,
		z0 = svnmla_n_f64_x (p0, z0, z1, 2),
		z0 = svnmla_x (p0, z0, z1, 2))

/*
** nmla_2_f64_x_tied2:
**	fmov	(z[0-9]+\.d), #2\.0(?:e\+0)?
**	fnmad	z0\.d, p0/m, \1, z1\.d
**	ret
*/
TEST_UNIFORM_Z (nmla_2_f64_x_tied2, svfloat64_t,
		z0 = svnmla_n_f64_x (p0, z1, z0, 2),
		z0 = svnmla_x (p0, z1, z0, 2))

/*
** nmla_2_f64_x_untied:
**	fmov	z0\.d, #2\.0(?:e\+0)?
**	fnmad	z0\.d, p0/m, z2\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (nmla_2_f64_x_untied, svfloat64_t,
		z0 = svnmla_n_f64_x (p0, z1, z2, 2),
		z0 = svnmla_x (p0, z1, z2, 2))

/*
** ptrue_nmla_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_nmla_f64_x_tied1, svfloat64_t,
		z0 = svnmla_f64_x (svptrue_b64 (), z0, z1, z2),
		z0 = svnmla_x (svptrue_b64 (), z0, z1, z2))

/*
** ptrue_nmla_f64_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_nmla_f64_x_tied2, svfloat64_t,
		z0 = svnmla_f64_x (svptrue_b64 (), z1, z0, z2),
		z0 = svnmla_x (svptrue_b64 (), z1, z0, z2))

/*
** ptrue_nmla_f64_x_tied3:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_nmla_f64_x_tied3, svfloat64_t,
		z0 = svnmla_f64_x (svptrue_b64 (), z1, z2, z0),
		z0 = svnmla_x (svptrue_b64 (), z1, z2, z0))

/*
** ptrue_nmla_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_nmla_f64_x_untied, svfloat64_t,
		z0 = svnmla_f64_x (svptrue_b64 (), z1, z2, z3),
		z0 = svnmla_x (svptrue_b64 (), z1, z2, z3))

/*
** ptrue_nmla_2_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_nmla_2_f64_x_tied1, svfloat64_t,
		z0 = svnmla_n_f64_x (svptrue_b64 (), z0, z1, 2),
		z0 = svnmla_x (svptrue_b64 (), z0, z1, 2))

/*
** ptrue_nmla_2_f64_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_nmla_2_f64_x_tied2, svfloat64_t,
		z0 = svnmla_n_f64_x (svptrue_b64 (), z1, z0, 2),
		z0 = svnmla_x (svptrue_b64 (), z1, z0, 2))

/*
** ptrue_nmla_2_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_nmla_2_f64_x_untied, svfloat64_t,
		z0 = svnmla_n_f64_x (svptrue_b64 (), z1, z2, 2),
		z0 = svnmla_x (svptrue_b64 (), z1, z2, 2))
