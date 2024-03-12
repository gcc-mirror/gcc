/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** nmls_f32_m_tied1:
**	fnmls	z0\.s, p0/m, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (nmls_f32_m_tied1, svfloat32_t,
		z0 = svnmls_f32_m (p0, z0, z1, z2),
		z0 = svnmls_m (p0, z0, z1, z2))

/*
** nmls_f32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fnmls	z0\.s, p0/m, \1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (nmls_f32_m_tied2, svfloat32_t,
		z0 = svnmls_f32_m (p0, z1, z0, z2),
		z0 = svnmls_m (p0, z1, z0, z2))

/*
** nmls_f32_m_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fnmls	z0\.s, p0/m, z2\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (nmls_f32_m_tied3, svfloat32_t,
		z0 = svnmls_f32_m (p0, z1, z2, z0),
		z0 = svnmls_m (p0, z1, z2, z0))

/*
** nmls_f32_m_untied:
**	movprfx	z0, z1
**	fnmls	z0\.s, p0/m, z2\.s, z3\.s
**	ret
*/
TEST_UNIFORM_Z (nmls_f32_m_untied, svfloat32_t,
		z0 = svnmls_f32_m (p0, z1, z2, z3),
		z0 = svnmls_m (p0, z1, z2, z3))

/*
** nmls_s4_f32_m_tied1:
**	mov	(z[0-9]+\.s), s4
**	fnmls	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (nmls_s4_f32_m_tied1, svfloat32_t, float,
		 z0 = svnmls_n_f32_m (p0, z0, z1, d4),
		 z0 = svnmls_m (p0, z0, z1, d4))

/*
** nmls_s4_f32_m_untied:
**	mov	(z[0-9]+\.s), s4
**	movprfx	z0, z1
**	fnmls	z0\.s, p0/m, z2\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (nmls_s4_f32_m_untied, svfloat32_t, float,
		 z0 = svnmls_n_f32_m (p0, z1, z2, d4),
		 z0 = svnmls_m (p0, z1, z2, d4))

/*
** nmls_2_f32_m_tied1:
**	fmov	(z[0-9]+\.s), #2\.0(?:e\+0)?
**	fnmls	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (nmls_2_f32_m_tied1, svfloat32_t,
		z0 = svnmls_n_f32_m (p0, z0, z1, 2),
		z0 = svnmls_m (p0, z0, z1, 2))

/*
** nmls_2_f32_m_untied:
**	fmov	(z[0-9]+\.s), #2\.0(?:e\+0)?
**	movprfx	z0, z1
**	fnmls	z0\.s, p0/m, z2\.s, \1
**	ret
*/
TEST_UNIFORM_Z (nmls_2_f32_m_untied, svfloat32_t,
		z0 = svnmls_n_f32_m (p0, z1, z2, 2),
		z0 = svnmls_m (p0, z1, z2, 2))

/*
** nmls_f32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	fnmls	z0\.s, p0/m, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (nmls_f32_z_tied1, svfloat32_t,
		z0 = svnmls_f32_z (p0, z0, z1, z2),
		z0 = svnmls_z (p0, z0, z1, z2))

/*
** nmls_f32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	fnmsb	z0\.s, p0/m, z2\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (nmls_f32_z_tied2, svfloat32_t,
		z0 = svnmls_f32_z (p0, z1, z0, z2),
		z0 = svnmls_z (p0, z1, z0, z2))

/*
** nmls_f32_z_tied3:
**	movprfx	z0\.s, p0/z, z0\.s
**	fnmsb	z0\.s, p0/m, z2\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (nmls_f32_z_tied3, svfloat32_t,
		z0 = svnmls_f32_z (p0, z1, z2, z0),
		z0 = svnmls_z (p0, z1, z2, z0))

/*
** nmls_f32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	fnmls	z0\.s, p0/m, z2\.s, z3\.s
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	fnmsb	z0\.s, p0/m, z3\.s, z1\.s
** |
**	movprfx	z0\.s, p0/z, z3\.s
**	fnmsb	z0\.s, p0/m, z2\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (nmls_f32_z_untied, svfloat32_t,
		z0 = svnmls_f32_z (p0, z1, z2, z3),
		z0 = svnmls_z (p0, z1, z2, z3))

/*
** nmls_s4_f32_z_tied1:
**	mov	(z[0-9]+\.s), s4
**	movprfx	z0\.s, p0/z, z0\.s
**	fnmls	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (nmls_s4_f32_z_tied1, svfloat32_t, float,
		 z0 = svnmls_n_f32_z (p0, z0, z1, d4),
		 z0 = svnmls_z (p0, z0, z1, d4))

/*
** nmls_s4_f32_z_tied2:
**	mov	(z[0-9]+\.s), s4
**	movprfx	z0\.s, p0/z, z0\.s
**	fnmsb	z0\.s, p0/m, \1, z1\.s
**	ret
*/
TEST_UNIFORM_ZD (nmls_s4_f32_z_tied2, svfloat32_t, float,
		 z0 = svnmls_n_f32_z (p0, z1, z0, d4),
		 z0 = svnmls_z (p0, z1, z0, d4))

/*
** nmls_s4_f32_z_untied:
**	mov	(z[0-9]+\.s), s4
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	fnmls	z0\.s, p0/m, z2\.s, \1
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	fnmsb	z0\.s, p0/m, \1, z1\.s
** |
**	movprfx	z0\.s, p0/z, \1
**	fnmsb	z0\.s, p0/m, z2\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZD (nmls_s4_f32_z_untied, svfloat32_t, float,
		 z0 = svnmls_n_f32_z (p0, z1, z2, d4),
		 z0 = svnmls_z (p0, z1, z2, d4))

/*
** nmls_2_f32_z_tied1:
**	fmov	(z[0-9]+\.s), #2\.0(?:e\+0)?
**	movprfx	z0\.s, p0/z, z0\.s
**	fnmls	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (nmls_2_f32_z_tied1, svfloat32_t,
		z0 = svnmls_n_f32_z (p0, z0, z1, 2),
		z0 = svnmls_z (p0, z0, z1, 2))

/*
** nmls_2_f32_z_tied2:
**	fmov	(z[0-9]+\.s), #2\.0(?:e\+0)?
**	movprfx	z0\.s, p0/z, z0\.s
**	fnmsb	z0\.s, p0/m, \1, z1\.s
**	ret
*/
TEST_UNIFORM_Z (nmls_2_f32_z_tied2, svfloat32_t,
		z0 = svnmls_n_f32_z (p0, z1, z0, 2),
		z0 = svnmls_z (p0, z1, z0, 2))

/*
** nmls_2_f32_z_untied:
**	fmov	(z[0-9]+\.s), #2\.0(?:e\+0)?
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	fnmls	z0\.s, p0/m, z2\.s, \1
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	fnmsb	z0\.s, p0/m, \1, z1\.s
** |
**	movprfx	z0\.s, p0/z, \1
**	fnmsb	z0\.s, p0/m, z2\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (nmls_2_f32_z_untied, svfloat32_t,
		z0 = svnmls_n_f32_z (p0, z1, z2, 2),
		z0 = svnmls_z (p0, z1, z2, 2))

/*
** nmls_f32_x_tied1:
**	fnmls	z0\.s, p0/m, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (nmls_f32_x_tied1, svfloat32_t,
		z0 = svnmls_f32_x (p0, z0, z1, z2),
		z0 = svnmls_x (p0, z0, z1, z2))

/*
** nmls_f32_x_tied2:
**	fnmsb	z0\.s, p0/m, z2\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (nmls_f32_x_tied2, svfloat32_t,
		z0 = svnmls_f32_x (p0, z1, z0, z2),
		z0 = svnmls_x (p0, z1, z0, z2))

/*
** nmls_f32_x_tied3:
**	fnmsb	z0\.s, p0/m, z2\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (nmls_f32_x_tied3, svfloat32_t,
		z0 = svnmls_f32_x (p0, z1, z2, z0),
		z0 = svnmls_x (p0, z1, z2, z0))

/*
** nmls_f32_x_untied:
** (
**	movprfx	z0, z1
**	fnmls	z0\.s, p0/m, z2\.s, z3\.s
** |
**	movprfx	z0, z2
**	fnmsb	z0\.s, p0/m, z3\.s, z1\.s
** |
**	movprfx	z0, z3
**	fnmsb	z0\.s, p0/m, z2\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (nmls_f32_x_untied, svfloat32_t,
		z0 = svnmls_f32_x (p0, z1, z2, z3),
		z0 = svnmls_x (p0, z1, z2, z3))

/*
** nmls_s4_f32_x_tied1:
**	mov	(z[0-9]+\.s), s4
**	fnmls	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (nmls_s4_f32_x_tied1, svfloat32_t, float,
		 z0 = svnmls_n_f32_x (p0, z0, z1, d4),
		 z0 = svnmls_x (p0, z0, z1, d4))

/*
** nmls_s4_f32_x_tied2:
**	mov	(z[0-9]+\.s), s4
**	fnmsb	z0\.s, p0/m, \1, z1\.s
**	ret
*/
TEST_UNIFORM_ZD (nmls_s4_f32_x_tied2, svfloat32_t, float,
		 z0 = svnmls_n_f32_x (p0, z1, z0, d4),
		 z0 = svnmls_x (p0, z1, z0, d4))

/*
** nmls_s4_f32_x_untied:
**	mov	z0\.s, s4
**	fnmsb	z0\.s, p0/m, z2\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZD (nmls_s4_f32_x_untied, svfloat32_t, float,
		 z0 = svnmls_n_f32_x (p0, z1, z2, d4),
		 z0 = svnmls_x (p0, z1, z2, d4))

/*
** nmls_2_f32_x_tied1:
**	fmov	(z[0-9]+\.s), #2\.0(?:e\+0)?
**	fnmls	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (nmls_2_f32_x_tied1, svfloat32_t,
		z0 = svnmls_n_f32_x (p0, z0, z1, 2),
		z0 = svnmls_x (p0, z0, z1, 2))

/*
** nmls_2_f32_x_tied2:
**	fmov	(z[0-9]+\.s), #2\.0(?:e\+0)?
**	fnmsb	z0\.s, p0/m, \1, z1\.s
**	ret
*/
TEST_UNIFORM_Z (nmls_2_f32_x_tied2, svfloat32_t,
		z0 = svnmls_n_f32_x (p0, z1, z0, 2),
		z0 = svnmls_x (p0, z1, z0, 2))

/*
** nmls_2_f32_x_untied:
**	fmov	z0\.s, #2\.0(?:e\+0)?
**	fnmsb	z0\.s, p0/m, z2\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (nmls_2_f32_x_untied, svfloat32_t,
		z0 = svnmls_n_f32_x (p0, z1, z2, 2),
		z0 = svnmls_x (p0, z1, z2, 2))

/*
** ptrue_nmls_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_nmls_f32_x_tied1, svfloat32_t,
		z0 = svnmls_f32_x (svptrue_b32 (), z0, z1, z2),
		z0 = svnmls_x (svptrue_b32 (), z0, z1, z2))

/*
** ptrue_nmls_f32_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_nmls_f32_x_tied2, svfloat32_t,
		z0 = svnmls_f32_x (svptrue_b32 (), z1, z0, z2),
		z0 = svnmls_x (svptrue_b32 (), z1, z0, z2))

/*
** ptrue_nmls_f32_x_tied3:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_nmls_f32_x_tied3, svfloat32_t,
		z0 = svnmls_f32_x (svptrue_b32 (), z1, z2, z0),
		z0 = svnmls_x (svptrue_b32 (), z1, z2, z0))

/*
** ptrue_nmls_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_nmls_f32_x_untied, svfloat32_t,
		z0 = svnmls_f32_x (svptrue_b32 (), z1, z2, z3),
		z0 = svnmls_x (svptrue_b32 (), z1, z2, z3))

/*
** ptrue_nmls_2_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_nmls_2_f32_x_tied1, svfloat32_t,
		z0 = svnmls_n_f32_x (svptrue_b32 (), z0, z1, 2),
		z0 = svnmls_x (svptrue_b32 (), z0, z1, 2))

/*
** ptrue_nmls_2_f32_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_nmls_2_f32_x_tied2, svfloat32_t,
		z0 = svnmls_n_f32_x (svptrue_b32 (), z1, z0, 2),
		z0 = svnmls_x (svptrue_b32 (), z1, z0, 2))

/*
** ptrue_nmls_2_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_nmls_2_f32_x_untied, svfloat32_t,
		z0 = svnmls_n_f32_x (svptrue_b32 (), z1, z2, 2),
		z0 = svnmls_x (svptrue_b32 (), z1, z2, 2))
