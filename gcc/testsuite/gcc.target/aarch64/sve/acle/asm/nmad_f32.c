/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** nmad_f32_m_tied1:
**	fnmad	z0\.s, p0/m, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (nmad_f32_m_tied1, svfloat32_t,
		z0 = svnmad_f32_m (p0, z0, z1, z2),
		z0 = svnmad_m (p0, z0, z1, z2))

/*
** nmad_f32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fnmad	z0\.s, p0/m, \1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (nmad_f32_m_tied2, svfloat32_t,
		z0 = svnmad_f32_m (p0, z1, z0, z2),
		z0 = svnmad_m (p0, z1, z0, z2))

/*
** nmad_f32_m_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fnmad	z0\.s, p0/m, z2\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (nmad_f32_m_tied3, svfloat32_t,
		z0 = svnmad_f32_m (p0, z1, z2, z0),
		z0 = svnmad_m (p0, z1, z2, z0))

/*
** nmad_f32_m_untied:
**	movprfx	z0, z1
**	fnmad	z0\.s, p0/m, z2\.s, z3\.s
**	ret
*/
TEST_UNIFORM_Z (nmad_f32_m_untied, svfloat32_t,
		z0 = svnmad_f32_m (p0, z1, z2, z3),
		z0 = svnmad_m (p0, z1, z2, z3))

/*
** nmad_s4_f32_m_tied1:
**	mov	(z[0-9]+\.s), s4
**	fnmad	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (nmad_s4_f32_m_tied1, svfloat32_t, float,
		 z0 = svnmad_n_f32_m (p0, z0, z1, d4),
		 z0 = svnmad_m (p0, z0, z1, d4))

/*
** nmad_s4_f32_m_untied:
**	mov	(z[0-9]+\.s), s4
**	movprfx	z0, z1
**	fnmad	z0\.s, p0/m, z2\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (nmad_s4_f32_m_untied, svfloat32_t, float,
		 z0 = svnmad_n_f32_m (p0, z1, z2, d4),
		 z0 = svnmad_m (p0, z1, z2, d4))

/*
** nmad_2_f32_m_tied1:
**	fmov	(z[0-9]+\.s), #2\.0(?:e\+0)?
**	fnmad	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (nmad_2_f32_m_tied1, svfloat32_t,
		z0 = svnmad_n_f32_m (p0, z0, z1, 2),
		z0 = svnmad_m (p0, z0, z1, 2))

/*
** nmad_2_f32_m_untied: { xfail *-*-* }
**	fmov	(z[0-9]+\.s), #2\.0(?:e\+0)?
**	movprfx	z0, z1
**	fnmad	z0\.s, p0/m, z2\.s, \1
**	ret
*/
TEST_UNIFORM_Z (nmad_2_f32_m_untied, svfloat32_t,
		z0 = svnmad_n_f32_m (p0, z1, z2, 2),
		z0 = svnmad_m (p0, z1, z2, 2))

/*
** nmad_f32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	fnmad	z0\.s, p0/m, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (nmad_f32_z_tied1, svfloat32_t,
		z0 = svnmad_f32_z (p0, z0, z1, z2),
		z0 = svnmad_z (p0, z0, z1, z2))

/*
** nmad_f32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	fnmad	z0\.s, p0/m, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (nmad_f32_z_tied2, svfloat32_t,
		z0 = svnmad_f32_z (p0, z1, z0, z2),
		z0 = svnmad_z (p0, z1, z0, z2))

/*
** nmad_f32_z_tied3:
**	movprfx	z0\.s, p0/z, z0\.s
**	fnmla	z0\.s, p0/m, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (nmad_f32_z_tied3, svfloat32_t,
		z0 = svnmad_f32_z (p0, z1, z2, z0),
		z0 = svnmad_z (p0, z1, z2, z0))

/*
** nmad_f32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	fnmad	z0\.s, p0/m, z2\.s, z3\.s
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	fnmad	z0\.s, p0/m, z1\.s, z3\.s
** |
**	movprfx	z0\.s, p0/z, z3\.s
**	fnmla	z0\.s, p0/m, z1\.s, z2\.s
** )
**	ret
*/
TEST_UNIFORM_Z (nmad_f32_z_untied, svfloat32_t,
		z0 = svnmad_f32_z (p0, z1, z2, z3),
		z0 = svnmad_z (p0, z1, z2, z3))

/*
** nmad_s4_f32_z_tied1:
**	mov	(z[0-9]+\.s), s4
**	movprfx	z0\.s, p0/z, z0\.s
**	fnmad	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (nmad_s4_f32_z_tied1, svfloat32_t, float,
		 z0 = svnmad_n_f32_z (p0, z0, z1, d4),
		 z0 = svnmad_z (p0, z0, z1, d4))

/*
** nmad_s4_f32_z_tied2:
**	mov	(z[0-9]+\.s), s4
**	movprfx	z0\.s, p0/z, z0\.s
**	fnmad	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (nmad_s4_f32_z_tied2, svfloat32_t, float,
		 z0 = svnmad_n_f32_z (p0, z1, z0, d4),
		 z0 = svnmad_z (p0, z1, z0, d4))

/*
** nmad_s4_f32_z_untied:
**	mov	(z[0-9]+\.s), s4
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	fnmad	z0\.s, p0/m, z2\.s, \1
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	fnmad	z0\.s, p0/m, z1\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	fnmla	z0\.s, p0/m, z1\.s, z2\.s
** )
**	ret
*/
TEST_UNIFORM_ZD (nmad_s4_f32_z_untied, svfloat32_t, float,
		 z0 = svnmad_n_f32_z (p0, z1, z2, d4),
		 z0 = svnmad_z (p0, z1, z2, d4))

/*
** nmad_2_f32_z_tied1:
**	fmov	(z[0-9]+\.s), #2\.0(?:e\+0)?
**	movprfx	z0\.s, p0/z, z0\.s
**	fnmad	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (nmad_2_f32_z_tied1, svfloat32_t,
		z0 = svnmad_n_f32_z (p0, z0, z1, 2),
		z0 = svnmad_z (p0, z0, z1, 2))

/*
** nmad_2_f32_z_tied2:
**	fmov	(z[0-9]+\.s), #2\.0(?:e\+0)?
**	movprfx	z0\.s, p0/z, z0\.s
**	fnmad	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (nmad_2_f32_z_tied2, svfloat32_t,
		z0 = svnmad_n_f32_z (p0, z1, z0, 2),
		z0 = svnmad_z (p0, z1, z0, 2))

/*
** nmad_2_f32_z_untied:
**	fmov	(z[0-9]+\.s), #2\.0(?:e\+0)?
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	fnmad	z0\.s, p0/m, z2\.s, \1
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	fnmad	z0\.s, p0/m, z1\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	fnmla	z0\.s, p0/m, z1\.s, z2\.s
** )
**	ret
*/
TEST_UNIFORM_Z (nmad_2_f32_z_untied, svfloat32_t,
		z0 = svnmad_n_f32_z (p0, z1, z2, 2),
		z0 = svnmad_z (p0, z1, z2, 2))

/*
** nmad_f32_x_tied1:
**	fnmad	z0\.s, p0/m, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (nmad_f32_x_tied1, svfloat32_t,
		z0 = svnmad_f32_x (p0, z0, z1, z2),
		z0 = svnmad_x (p0, z0, z1, z2))

/*
** nmad_f32_x_tied2:
**	fnmad	z0\.s, p0/m, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (nmad_f32_x_tied2, svfloat32_t,
		z0 = svnmad_f32_x (p0, z1, z0, z2),
		z0 = svnmad_x (p0, z1, z0, z2))

/*
** nmad_f32_x_tied3:
**	fnmla	z0\.s, p0/m, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (nmad_f32_x_tied3, svfloat32_t,
		z0 = svnmad_f32_x (p0, z1, z2, z0),
		z0 = svnmad_x (p0, z1, z2, z0))

/*
** nmad_f32_x_untied:
** (
**	movprfx	z0, z1
**	fnmad	z0\.s, p0/m, z2\.s, z3\.s
** |
**	movprfx	z0, z2
**	fnmad	z0\.s, p0/m, z1\.s, z3\.s
** |
**	movprfx	z0, z3
**	fnmla	z0\.s, p0/m, z1\.s, z2\.s
** )
**	ret
*/
TEST_UNIFORM_Z (nmad_f32_x_untied, svfloat32_t,
		z0 = svnmad_f32_x (p0, z1, z2, z3),
		z0 = svnmad_x (p0, z1, z2, z3))

/*
** nmad_s4_f32_x_tied1:
**	mov	(z[0-9]+\.s), s4
**	fnmad	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (nmad_s4_f32_x_tied1, svfloat32_t, float,
		 z0 = svnmad_n_f32_x (p0, z0, z1, d4),
		 z0 = svnmad_x (p0, z0, z1, d4))

/*
** nmad_s4_f32_x_tied2:
**	mov	(z[0-9]+\.s), s4
**	fnmad	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (nmad_s4_f32_x_tied2, svfloat32_t, float,
		 z0 = svnmad_n_f32_x (p0, z1, z0, d4),
		 z0 = svnmad_x (p0, z1, z0, d4))

/*
** nmad_s4_f32_x_untied:
**	mov	z0\.s, s4
**	fnmla	z0\.s, p0/m, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_ZD (nmad_s4_f32_x_untied, svfloat32_t, float,
		 z0 = svnmad_n_f32_x (p0, z1, z2, d4),
		 z0 = svnmad_x (p0, z1, z2, d4))

/*
** nmad_2_f32_x_tied1:
**	fmov	(z[0-9]+\.s), #2\.0(?:e\+0)?
**	fnmad	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (nmad_2_f32_x_tied1, svfloat32_t,
		z0 = svnmad_n_f32_x (p0, z0, z1, 2),
		z0 = svnmad_x (p0, z0, z1, 2))

/*
** nmad_2_f32_x_tied2:
**	fmov	(z[0-9]+\.s), #2\.0(?:e\+0)?
**	fnmad	z0\.s, p0/m, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (nmad_2_f32_x_tied2, svfloat32_t,
		z0 = svnmad_n_f32_x (p0, z1, z0, 2),
		z0 = svnmad_x (p0, z1, z0, 2))

/*
** nmad_2_f32_x_untied:
**	fmov	z0\.s, #2\.0(?:e\+0)?
**	fnmla	z0\.s, p0/m, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (nmad_2_f32_x_untied, svfloat32_t,
		z0 = svnmad_n_f32_x (p0, z1, z2, 2),
		z0 = svnmad_x (p0, z1, z2, 2))

/*
** ptrue_nmad_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_nmad_f32_x_tied1, svfloat32_t,
		z0 = svnmad_f32_x (svptrue_b32 (), z0, z1, z2),
		z0 = svnmad_x (svptrue_b32 (), z0, z1, z2))

/*
** ptrue_nmad_f32_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_nmad_f32_x_tied2, svfloat32_t,
		z0 = svnmad_f32_x (svptrue_b32 (), z1, z0, z2),
		z0 = svnmad_x (svptrue_b32 (), z1, z0, z2))

/*
** ptrue_nmad_f32_x_tied3:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_nmad_f32_x_tied3, svfloat32_t,
		z0 = svnmad_f32_x (svptrue_b32 (), z1, z2, z0),
		z0 = svnmad_x (svptrue_b32 (), z1, z2, z0))

/*
** ptrue_nmad_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_nmad_f32_x_untied, svfloat32_t,
		z0 = svnmad_f32_x (svptrue_b32 (), z1, z2, z3),
		z0 = svnmad_x (svptrue_b32 (), z1, z2, z3))

/*
** ptrue_nmad_2_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_nmad_2_f32_x_tied1, svfloat32_t,
		z0 = svnmad_n_f32_x (svptrue_b32 (), z0, z1, 2),
		z0 = svnmad_x (svptrue_b32 (), z0, z1, 2))

/*
** ptrue_nmad_2_f32_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_nmad_2_f32_x_tied2, svfloat32_t,
		z0 = svnmad_n_f32_x (svptrue_b32 (), z1, z0, 2),
		z0 = svnmad_x (svptrue_b32 (), z1, z0, 2))

/*
** ptrue_nmad_2_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_nmad_2_f32_x_untied, svfloat32_t,
		z0 = svnmad_n_f32_x (svptrue_b32 (), z1, z2, 2),
		z0 = svnmad_x (svptrue_b32 (), z1, z2, 2))
