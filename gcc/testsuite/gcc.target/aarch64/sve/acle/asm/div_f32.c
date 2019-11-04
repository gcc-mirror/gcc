/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** div_f32_m_tied1:
**	fdiv	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (div_f32_m_tied1, svfloat32_t,
		z0 = svdiv_f32_m (p0, z0, z1),
		z0 = svdiv_m (p0, z0, z1))

/*
** div_f32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fdiv	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (div_f32_m_tied2, svfloat32_t,
		z0 = svdiv_f32_m (p0, z1, z0),
		z0 = svdiv_m (p0, z1, z0))

/*
** div_f32_m_untied:
**	movprfx	z0, z1
**	fdiv	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (div_f32_m_untied, svfloat32_t,
		z0 = svdiv_f32_m (p0, z1, z2),
		z0 = svdiv_m (p0, z1, z2))

/*
** div_s4_f32_m_tied1:
**	mov	(z[0-9]+\.s), s4
**	fdiv	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (div_s4_f32_m_tied1, svfloat32_t, float,
		 z0 = svdiv_n_f32_m (p0, z0, d4),
		 z0 = svdiv_m (p0, z0, d4))

/*
** div_s4_f32_m_untied:
**	mov	(z[0-9]+\.s), s4
**	movprfx	z0, z1
**	fdiv	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (div_s4_f32_m_untied, svfloat32_t, float,
		 z0 = svdiv_n_f32_m (p0, z1, d4),
		 z0 = svdiv_m (p0, z1, d4))

/*
** div_1_f32_m_tied1:
**	fmov	(z[0-9]+\.s), #1\.0(?:e\+0)?
**	fdiv	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (div_1_f32_m_tied1, svfloat32_t,
		z0 = svdiv_n_f32_m (p0, z0, 1),
		z0 = svdiv_m (p0, z0, 1))

/*
** div_1_f32_m_untied: { xfail *-*-* }
**	fmov	(z[0-9]+\.s), #1\.0(?:e\+0)?
**	movprfx	z0, z1
**	fdiv	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (div_1_f32_m_untied, svfloat32_t,
		z0 = svdiv_n_f32_m (p0, z1, 1),
		z0 = svdiv_m (p0, z1, 1))

/*
** div_f32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	fdiv	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (div_f32_z_tied1, svfloat32_t,
		z0 = svdiv_f32_z (p0, z0, z1),
		z0 = svdiv_z (p0, z0, z1))

/*
** div_f32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	fdivr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (div_f32_z_tied2, svfloat32_t,
		z0 = svdiv_f32_z (p0, z1, z0),
		z0 = svdiv_z (p0, z1, z0))

/*
** div_f32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	fdiv	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	fdivr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (div_f32_z_untied, svfloat32_t,
		z0 = svdiv_f32_z (p0, z1, z2),
		z0 = svdiv_z (p0, z1, z2))

/*
** div_s4_f32_z_tied1:
**	mov	(z[0-9]+\.s), s4
**	movprfx	z0\.s, p0/z, z0\.s
**	fdiv	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (div_s4_f32_z_tied1, svfloat32_t, float,
		 z0 = svdiv_n_f32_z (p0, z0, d4),
		 z0 = svdiv_z (p0, z0, d4))

/*
** div_s4_f32_z_untied:
**	mov	(z[0-9]+\.s), s4
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	fdiv	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	fdivr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZD (div_s4_f32_z_untied, svfloat32_t, float,
		 z0 = svdiv_n_f32_z (p0, z1, d4),
		 z0 = svdiv_z (p0, z1, d4))

/*
** div_1_f32_z_tied1:
**	fmov	(z[0-9]+\.s), #1\.0(?:e\+0)?
**	movprfx	z0\.s, p0/z, z0\.s
**	fdiv	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (div_1_f32_z_tied1, svfloat32_t,
		z0 = svdiv_n_f32_z (p0, z0, 1),
		z0 = svdiv_z (p0, z0, 1))

/*
** div_1_f32_z_untied:
**	fmov	(z[0-9]+\.s), #1\.0(?:e\+0)?
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	fdiv	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	fdivr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (div_1_f32_z_untied, svfloat32_t,
		z0 = svdiv_n_f32_z (p0, z1, 1),
		z0 = svdiv_z (p0, z1, 1))

/*
** div_0p5_f32_z:
**	fmov	(z[0-9]+\.s), #(?:0\.5|5\.0e-1)
**	movprfx	z0\.s, p0/z, z0\.s
**	fdiv	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (div_0p5_f32_z, svfloat32_t,
		z0 = svdiv_n_f32_z (p0, z0, 0.5),
		z0 = svdiv_z (p0, z0, 0.5))

/*
** div_f32_x_tied1:
**	fdiv	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (div_f32_x_tied1, svfloat32_t,
		z0 = svdiv_f32_x (p0, z0, z1),
		z0 = svdiv_x (p0, z0, z1))

/*
** div_f32_x_tied2:
**	fdivr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (div_f32_x_tied2, svfloat32_t,
		z0 = svdiv_f32_x (p0, z1, z0),
		z0 = svdiv_x (p0, z1, z0))

/*
** div_f32_x_untied:
** (
**	movprfx	z0, z1
**	fdiv	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0, z2
**	fdivr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (div_f32_x_untied, svfloat32_t,
		z0 = svdiv_f32_x (p0, z1, z2),
		z0 = svdiv_x (p0, z1, z2))

/*
** div_s4_f32_x_tied1:
**	mov	(z[0-9]+\.s), s4
**	fdiv	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (div_s4_f32_x_tied1, svfloat32_t, float,
		 z0 = svdiv_n_f32_x (p0, z0, d4),
		 z0 = svdiv_x (p0, z0, d4))

/*
** div_s4_f32_x_untied: { xfail *-*-* }
**	mov	z0\.s, s4
**	fdivr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZD (div_s4_f32_x_untied, svfloat32_t, float,
		 z0 = svdiv_n_f32_x (p0, z1, d4),
		 z0 = svdiv_x (p0, z1, d4))

/*
** div_1_f32_x_tied1:
**	fmov	(z[0-9]+\.s), #1\.0(?:e\+0)?
**	fdiv	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (div_1_f32_x_tied1, svfloat32_t,
		z0 = svdiv_n_f32_x (p0, z0, 1),
		z0 = svdiv_x (p0, z0, 1))

/*
** div_1_f32_x_untied:
**	fmov	z0\.s, #1\.0(?:e\+0)?
**	fdivr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (div_1_f32_x_untied, svfloat32_t,
		z0 = svdiv_n_f32_x (p0, z1, 1),
		z0 = svdiv_x (p0, z1, 1))

/*
** ptrue_div_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_div_f32_x_tied1, svfloat32_t,
		z0 = svdiv_f32_x (svptrue_b32 (), z0, z1),
		z0 = svdiv_x (svptrue_b32 (), z0, z1))

/*
** ptrue_div_f32_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_div_f32_x_tied2, svfloat32_t,
		z0 = svdiv_f32_x (svptrue_b32 (), z1, z0),
		z0 = svdiv_x (svptrue_b32 (), z1, z0))

/*
** ptrue_div_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_div_f32_x_untied, svfloat32_t,
		z0 = svdiv_f32_x (svptrue_b32 (), z1, z2),
		z0 = svdiv_x (svptrue_b32 (), z1, z2))

/*
** ptrue_div_1_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_div_1_f32_x_tied1, svfloat32_t,
		z0 = svdiv_n_f32_x (svptrue_b32 (), z0, 1),
		z0 = svdiv_x (svptrue_b32 (), z0, 1))

/*
** ptrue_div_1_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_div_1_f32_x_untied, svfloat32_t,
		z0 = svdiv_n_f32_x (svptrue_b32 (), z1, 1),
		z0 = svdiv_x (svptrue_b32 (), z1, 1))
