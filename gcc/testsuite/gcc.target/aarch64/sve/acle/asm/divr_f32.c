/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** divr_f32_m_tied1:
**	fdivr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (divr_f32_m_tied1, svfloat32_t,
		z0 = svdivr_f32_m (p0, z0, z1),
		z0 = svdivr_m (p0, z0, z1))

/*
** divr_f32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fdivr	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (divr_f32_m_tied2, svfloat32_t,
		z0 = svdivr_f32_m (p0, z1, z0),
		z0 = svdivr_m (p0, z1, z0))

/*
** divr_f32_m_untied:
**	movprfx	z0, z1
**	fdivr	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (divr_f32_m_untied, svfloat32_t,
		z0 = svdivr_f32_m (p0, z1, z2),
		z0 = svdivr_m (p0, z1, z2))

/*
** divr_s4_f32_m_tied1:
**	mov	(z[0-9]+\.s), s4
**	fdivr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (divr_s4_f32_m_tied1, svfloat32_t, float,
		 z0 = svdivr_n_f32_m (p0, z0, d4),
		 z0 = svdivr_m (p0, z0, d4))

/*
** divr_s4_f32_m_untied:
**	mov	(z[0-9]+\.s), s4
**	movprfx	z0, z1
**	fdivr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (divr_s4_f32_m_untied, svfloat32_t, float,
		 z0 = svdivr_n_f32_m (p0, z1, d4),
		 z0 = svdivr_m (p0, z1, d4))

/*
** divr_1_f32_m_tied1:
**	fmov	(z[0-9]+\.s), #1\.0(?:e\+0)?
**	fdivr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (divr_1_f32_m_tied1, svfloat32_t,
		z0 = svdivr_n_f32_m (p0, z0, 1),
		z0 = svdivr_m (p0, z0, 1))

/*
** divr_1_f32_m_untied:
**	fmov	(z[0-9]+\.s), #1\.0(?:e\+0)?
**	movprfx	z0, z1
**	fdivr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (divr_1_f32_m_untied, svfloat32_t,
		z0 = svdivr_n_f32_m (p0, z1, 1),
		z0 = svdivr_m (p0, z1, 1))

/*
** divr_0p5_f32_m_tied1:
**	fmov	(z[0-9]+\.s), #(?:0\.5|5\.0e-1)
**	fdivr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (divr_0p5_f32_m_tied1, svfloat32_t,
		z0 = svdivr_n_f32_m (p0, z0, 0.5),
		z0 = svdivr_m (p0, z0, 0.5))

/*
** divr_0p5_f32_m_untied:
**	fmov	(z[0-9]+\.s), #(?:0\.5|5\.0e-1)
**	movprfx	z0, z1
**	fdivr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (divr_0p5_f32_m_untied, svfloat32_t,
		z0 = svdivr_n_f32_m (p0, z1, 0.5),
		z0 = svdivr_m (p0, z1, 0.5))

/*
** divr_f32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	fdivr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (divr_f32_z_tied1, svfloat32_t,
		z0 = svdivr_f32_z (p0, z0, z1),
		z0 = svdivr_z (p0, z0, z1))

/*
** divr_f32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	fdiv	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (divr_f32_z_tied2, svfloat32_t,
		z0 = svdivr_f32_z (p0, z1, z0),
		z0 = svdivr_z (p0, z1, z0))

/*
** divr_f32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	fdivr	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	fdiv	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (divr_f32_z_untied, svfloat32_t,
		z0 = svdivr_f32_z (p0, z1, z2),
		z0 = svdivr_z (p0, z1, z2))

/*
** divr_s4_f32_z_tied1:
**	mov	(z[0-9]+\.s), s4
**	movprfx	z0\.s, p0/z, z0\.s
**	fdivr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (divr_s4_f32_z_tied1, svfloat32_t, float,
		 z0 = svdivr_n_f32_z (p0, z0, d4),
		 z0 = svdivr_z (p0, z0, d4))

/*
** divr_s4_f32_z_untied:
**	mov	(z[0-9]+\.s), s4
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	fdivr	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	fdiv	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZD (divr_s4_f32_z_untied, svfloat32_t, float,
		 z0 = svdivr_n_f32_z (p0, z1, d4),
		 z0 = svdivr_z (p0, z1, d4))

/*
** divr_1_f32_z:
**	fmov	(z[0-9]+\.s), #1\.0(?:e\+0)?
**	movprfx	z0\.s, p0/z, z0\.s
**	fdivr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (divr_1_f32_z, svfloat32_t,
		z0 = svdivr_n_f32_z (p0, z0, 1),
		z0 = svdivr_z (p0, z0, 1))

/*
** divr_0p5_f32_z_tied1:
**	fmov	(z[0-9]+\.s), #(?:0\.5|5\.0e-1)
**	movprfx	z0\.s, p0/z, z0\.s
**	fdivr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (divr_0p5_f32_z_tied1, svfloat32_t,
		z0 = svdivr_n_f32_z (p0, z0, 0.5),
		z0 = svdivr_z (p0, z0, 0.5))

/*
** divr_0p5_f32_z_untied:
**	fmov	(z[0-9]+\.s), #(?:0\.5|5\.0e-1)
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	fdivr	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	fdiv	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (divr_0p5_f32_z_untied, svfloat32_t,
		z0 = svdivr_n_f32_z (p0, z1, 0.5),
		z0 = svdivr_z (p0, z1, 0.5))

/*
** divr_f32_x_tied1:
**	fdivr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (divr_f32_x_tied1, svfloat32_t,
		z0 = svdivr_f32_x (p0, z0, z1),
		z0 = svdivr_x (p0, z0, z1))

/*
** divr_f32_x_tied2:
**	fdiv	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (divr_f32_x_tied2, svfloat32_t,
		z0 = svdivr_f32_x (p0, z1, z0),
		z0 = svdivr_x (p0, z1, z0))

/*
** divr_f32_x_untied:
** (
**	movprfx	z0, z1
**	fdivr	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0, z2
**	fdiv	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (divr_f32_x_untied, svfloat32_t,
		z0 = svdivr_f32_x (p0, z1, z2),
		z0 = svdivr_x (p0, z1, z2))

/*
** divr_s4_f32_x_tied1:
**	mov	(z[0-9]+\.s), s4
**	fdivr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (divr_s4_f32_x_tied1, svfloat32_t, float,
		 z0 = svdivr_n_f32_x (p0, z0, d4),
		 z0 = svdivr_x (p0, z0, d4))

/*
** divr_s4_f32_x_untied:
**	mov	z0\.s, s4
**	fdiv	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZD (divr_s4_f32_x_untied, svfloat32_t, float,
		 z0 = svdivr_n_f32_x (p0, z1, d4),
		 z0 = svdivr_x (p0, z1, d4))

/*
** divr_1_f32_x_tied1:
**	fmov	(z[0-9]+\.s), #1\.0(?:e\+0)?
**	fdivr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (divr_1_f32_x_tied1, svfloat32_t,
		z0 = svdivr_n_f32_x (p0, z0, 1),
		z0 = svdivr_x (p0, z0, 1))

/*
** divr_1_f32_x_untied:
**	fmov	z0\.s, #1\.0(?:e\+0)?
**	fdiv	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (divr_1_f32_x_untied, svfloat32_t,
		z0 = svdivr_n_f32_x (p0, z1, 1),
		z0 = svdivr_x (p0, z1, 1))

/*
** ptrue_divr_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_divr_f32_x_tied1, svfloat32_t,
		z0 = svdivr_f32_x (svptrue_b32 (), z0, z1),
		z0 = svdivr_x (svptrue_b32 (), z0, z1))

/*
** ptrue_divr_f32_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_divr_f32_x_tied2, svfloat32_t,
		z0 = svdivr_f32_x (svptrue_b32 (), z1, z0),
		z0 = svdivr_x (svptrue_b32 (), z1, z0))

/*
** ptrue_divr_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_divr_f32_x_untied, svfloat32_t,
		z0 = svdivr_f32_x (svptrue_b32 (), z1, z2),
		z0 = svdivr_x (svptrue_b32 (), z1, z2))

/*
** ptrue_divr_1_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_divr_1_f32_x_tied1, svfloat32_t,
		z0 = svdivr_n_f32_x (svptrue_b32 (), z0, 1),
		z0 = svdivr_x (svptrue_b32 (), z0, 1))

/*
** ptrue_divr_1_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_divr_1_f32_x_untied, svfloat32_t,
		z0 = svdivr_n_f32_x (svptrue_b32 (), z1, 1),
		z0 = svdivr_x (svptrue_b32 (), z1, 1))
