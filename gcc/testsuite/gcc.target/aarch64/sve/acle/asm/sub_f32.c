/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sub_f32_m_tied1:
**	fsub	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (sub_f32_m_tied1, svfloat32_t,
		z0 = svsub_f32_m (p0, z0, z1),
		z0 = svsub_m (p0, z0, z1))

/*
** sub_f32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fsub	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (sub_f32_m_tied2, svfloat32_t,
		z0 = svsub_f32_m (p0, z1, z0),
		z0 = svsub_m (p0, z1, z0))

/*
** sub_f32_m_untied:
**	movprfx	z0, z1
**	fsub	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (sub_f32_m_untied, svfloat32_t,
		z0 = svsub_f32_m (p0, z1, z2),
		z0 = svsub_m (p0, z1, z2))

/*
** sub_s4_f32_m_tied1:
**	mov	(z[0-9]+\.s), s4
**	fsub	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (sub_s4_f32_m_tied1, svfloat32_t, float,
		 z0 = svsub_n_f32_m (p0, z0, d4),
		 z0 = svsub_m (p0, z0, d4))

/*
** sub_s4_f32_m_untied:
**	mov	(z[0-9]+\.s), s4
**	movprfx	z0, z1
**	fsub	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (sub_s4_f32_m_untied, svfloat32_t, float,
		 z0 = svsub_n_f32_m (p0, z1, d4),
		 z0 = svsub_m (p0, z1, d4))

/*
** sub_1_f32_m_tied1:
**	fsub	z0\.s, p0/m, z0\.s, #1\.0
**	ret
*/
TEST_UNIFORM_Z (sub_1_f32_m_tied1, svfloat32_t,
		z0 = svsub_n_f32_m (p0, z0, 1),
		z0 = svsub_m (p0, z0, 1))

/*
** sub_1_f32_m_untied:
**	movprfx	z0, z1
**	fsub	z0\.s, p0/m, z0\.s, #1\.0
**	ret
*/
TEST_UNIFORM_Z (sub_1_f32_m_untied, svfloat32_t,
		z0 = svsub_n_f32_m (p0, z1, 1),
		z0 = svsub_m (p0, z1, 1))

/*
** sub_0p5_f32_m_tied1:
**	fsub	z0\.s, p0/m, z0\.s, #0\.5
**	ret
*/
TEST_UNIFORM_Z (sub_0p5_f32_m_tied1, svfloat32_t,
		z0 = svsub_n_f32_m (p0, z0, 0.5),
		z0 = svsub_m (p0, z0, 0.5))

/*
** sub_0p5_f32_m_untied:
**	movprfx	z0, z1
**	fsub	z0\.s, p0/m, z0\.s, #0\.5
**	ret
*/
TEST_UNIFORM_Z (sub_0p5_f32_m_untied, svfloat32_t,
		z0 = svsub_n_f32_m (p0, z1, 0.5),
		z0 = svsub_m (p0, z1, 0.5))

/*
** sub_m1_f32_m_tied1:
**	fadd	z0\.s, p0/m, z0\.s, #1\.0
**	ret
*/
TEST_UNIFORM_Z (sub_m1_f32_m_tied1, svfloat32_t,
		z0 = svsub_n_f32_m (p0, z0, -1),
		z0 = svsub_m (p0, z0, -1))

/*
** sub_m1_f32_m_untied:
**	movprfx	z0, z1
**	fadd	z0\.s, p0/m, z0\.s, #1\.0
**	ret
*/
TEST_UNIFORM_Z (sub_m1_f32_m_untied, svfloat32_t,
		z0 = svsub_n_f32_m (p0, z1, -1),
		z0 = svsub_m (p0, z1, -1))

/*
** sub_m0p5_f32_m_tied1:
**	fadd	z0\.s, p0/m, z0\.s, #0\.5
**	ret
*/
TEST_UNIFORM_Z (sub_m0p5_f32_m_tied1, svfloat32_t,
		z0 = svsub_n_f32_m (p0, z0, -0.5),
		z0 = svsub_m (p0, z0, -0.5))

/*
** sub_m0p5_f32_m_untied:
**	movprfx	z0, z1
**	fadd	z0\.s, p0/m, z0\.s, #0\.5
**	ret
*/
TEST_UNIFORM_Z (sub_m0p5_f32_m_untied, svfloat32_t,
		z0 = svsub_n_f32_m (p0, z1, -0.5),
		z0 = svsub_m (p0, z1, -0.5))

/*
** sub_m2_f32_m:
**	fmov	(z[0-9]+\.s), #2\.0(?:e\+0)?
**	fadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (sub_m2_f32_m, svfloat32_t,
		z0 = svsub_n_f32_m (p0, z0, -2),
		z0 = svsub_m (p0, z0, -2))

/*
** sub_f32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	fsub	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (sub_f32_z_tied1, svfloat32_t,
		z0 = svsub_f32_z (p0, z0, z1),
		z0 = svsub_z (p0, z0, z1))

/*
** sub_f32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	fsubr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (sub_f32_z_tied2, svfloat32_t,
		z0 = svsub_f32_z (p0, z1, z0),
		z0 = svsub_z (p0, z1, z0))

/*
** sub_f32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	fsub	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	fsubr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (sub_f32_z_untied, svfloat32_t,
		z0 = svsub_f32_z (p0, z1, z2),
		z0 = svsub_z (p0, z1, z2))

/*
** sub_s4_f32_z_tied1:
**	mov	(z[0-9]+\.s), s4
**	movprfx	z0\.s, p0/z, z0\.s
**	fsub	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (sub_s4_f32_z_tied1, svfloat32_t, float,
		 z0 = svsub_n_f32_z (p0, z0, d4),
		 z0 = svsub_z (p0, z0, d4))

/*
** sub_s4_f32_z_untied:
**	mov	(z[0-9]+\.s), s4
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	fsub	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	fsubr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZD (sub_s4_f32_z_untied, svfloat32_t, float,
		 z0 = svsub_n_f32_z (p0, z1, d4),
		 z0 = svsub_z (p0, z1, d4))

/*
** sub_1_f32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	fsub	z0\.s, p0/m, z0\.s, #1\.0
**	ret
*/
TEST_UNIFORM_Z (sub_1_f32_z_tied1, svfloat32_t,
		z0 = svsub_n_f32_z (p0, z0, 1),
		z0 = svsub_z (p0, z0, 1))

/*
** sub_1_f32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	fsub	z0\.s, p0/m, z0\.s, #1\.0
**	ret
*/
TEST_UNIFORM_Z (sub_1_f32_z_untied, svfloat32_t,
		z0 = svsub_n_f32_z (p0, z1, 1),
		z0 = svsub_z (p0, z1, 1))

/*
** sub_0p5_f32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	fsub	z0\.s, p0/m, z0\.s, #0\.5
**	ret
*/
TEST_UNIFORM_Z (sub_0p5_f32_z_tied1, svfloat32_t,
		z0 = svsub_n_f32_z (p0, z0, 0.5),
		z0 = svsub_z (p0, z0, 0.5))

/*
** sub_0p5_f32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	fsub	z0\.s, p0/m, z0\.s, #0\.5
**	ret
*/
TEST_UNIFORM_Z (sub_0p5_f32_z_untied, svfloat32_t,
		z0 = svsub_n_f32_z (p0, z1, 0.5),
		z0 = svsub_z (p0, z1, 0.5))

/*
** sub_m1_f32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	fadd	z0\.s, p0/m, z0\.s, #1\.0
**	ret
*/
TEST_UNIFORM_Z (sub_m1_f32_z_tied1, svfloat32_t,
		z0 = svsub_n_f32_z (p0, z0, -1),
		z0 = svsub_z (p0, z0, -1))

/*
** sub_m1_f32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	fadd	z0\.s, p0/m, z0\.s, #1\.0
**	ret
*/
TEST_UNIFORM_Z (sub_m1_f32_z_untied, svfloat32_t,
		z0 = svsub_n_f32_z (p0, z1, -1),
		z0 = svsub_z (p0, z1, -1))

/*
** sub_m0p5_f32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	fadd	z0\.s, p0/m, z0\.s, #0\.5
**	ret
*/
TEST_UNIFORM_Z (sub_m0p5_f32_z_tied1, svfloat32_t,
		z0 = svsub_n_f32_z (p0, z0, -0.5),
		z0 = svsub_z (p0, z0, -0.5))

/*
** sub_m0p5_f32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	fadd	z0\.s, p0/m, z0\.s, #0\.5
**	ret
*/
TEST_UNIFORM_Z (sub_m0p5_f32_z_untied, svfloat32_t,
		z0 = svsub_n_f32_z (p0, z1, -0.5),
		z0 = svsub_z (p0, z1, -0.5))

/*
** sub_m2_f32_z:
**	fmov	(z[0-9]+\.s), #2\.0(?:e\+0)?
**	movprfx	z0\.s, p0/z, z0\.s
**	fadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (sub_m2_f32_z, svfloat32_t,
		z0 = svsub_n_f32_z (p0, z0, -2),
		z0 = svsub_z (p0, z0, -2))

/*
** sub_f32_x_tied1:
**	fsub	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (sub_f32_x_tied1, svfloat32_t,
		z0 = svsub_f32_x (p0, z0, z1),
		z0 = svsub_x (p0, z0, z1))

/*
** sub_f32_x_tied2:
**	fsubr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (sub_f32_x_tied2, svfloat32_t,
		z0 = svsub_f32_x (p0, z1, z0),
		z0 = svsub_x (p0, z1, z0))

/*
** sub_f32_x_untied:
** (
**	movprfx	z0, z1
**	fsub	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0, z2
**	fsubr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (sub_f32_x_untied, svfloat32_t,
		z0 = svsub_f32_x (p0, z1, z2),
		z0 = svsub_x (p0, z1, z2))

/*
** sub_s4_f32_x_tied1:
**	mov	(z[0-9]+\.s), s4
**	fsub	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (sub_s4_f32_x_tied1, svfloat32_t, float,
		 z0 = svsub_n_f32_x (p0, z0, d4),
		 z0 = svsub_x (p0, z0, d4))

/*
** sub_s4_f32_x_untied:
**	mov	z0\.s, s4
**	fsubr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZD (sub_s4_f32_x_untied, svfloat32_t, float,
		 z0 = svsub_n_f32_x (p0, z1, d4),
		 z0 = svsub_x (p0, z1, d4))

/*
** sub_1_f32_x_tied1:
**	fsub	z0\.s, p0/m, z0\.s, #1\.0
**	ret
*/
TEST_UNIFORM_Z (sub_1_f32_x_tied1, svfloat32_t,
		z0 = svsub_n_f32_x (p0, z0, 1),
		z0 = svsub_x (p0, z0, 1))

/*
** sub_1_f32_x_untied:
**	movprfx	z0, z1
**	fsub	z0\.s, p0/m, z0\.s, #1\.0
**	ret
*/
TEST_UNIFORM_Z (sub_1_f32_x_untied, svfloat32_t,
		z0 = svsub_n_f32_x (p0, z1, 1),
		z0 = svsub_x (p0, z1, 1))

/*
** sub_0p5_f32_x_tied1:
**	fsub	z0\.s, p0/m, z0\.s, #0\.5
**	ret
*/
TEST_UNIFORM_Z (sub_0p5_f32_x_tied1, svfloat32_t,
		z0 = svsub_n_f32_x (p0, z0, 0.5),
		z0 = svsub_x (p0, z0, 0.5))

/*
** sub_0p5_f32_x_untied:
**	movprfx	z0, z1
**	fsub	z0\.s, p0/m, z0\.s, #0\.5
**	ret
*/
TEST_UNIFORM_Z (sub_0p5_f32_x_untied, svfloat32_t,
		z0 = svsub_n_f32_x (p0, z1, 0.5),
		z0 = svsub_x (p0, z1, 0.5))

/*
** sub_m1_f32_x_tied1:
**	fadd	z0\.s, p0/m, z0\.s, #1\.0
**	ret
*/
TEST_UNIFORM_Z (sub_m1_f32_x_tied1, svfloat32_t,
		z0 = svsub_n_f32_x (p0, z0, -1),
		z0 = svsub_x (p0, z0, -1))

/*
** sub_m1_f32_x_untied:
**	movprfx	z0, z1
**	fadd	z0\.s, p0/m, z0\.s, #1\.0
**	ret
*/
TEST_UNIFORM_Z (sub_m1_f32_x_untied, svfloat32_t,
		z0 = svsub_n_f32_x (p0, z1, -1),
		z0 = svsub_x (p0, z1, -1))

/*
** sub_m0p5_f32_x_tied1:
**	fadd	z0\.s, p0/m, z0\.s, #0\.5
**	ret
*/
TEST_UNIFORM_Z (sub_m0p5_f32_x_tied1, svfloat32_t,
		z0 = svsub_n_f32_x (p0, z0, -0.5),
		z0 = svsub_x (p0, z0, -0.5))

/*
** sub_m0p5_f32_x_untied:
**	movprfx	z0, z1
**	fadd	z0\.s, p0/m, z0\.s, #0\.5
**	ret
*/
TEST_UNIFORM_Z (sub_m0p5_f32_x_untied, svfloat32_t,
		z0 = svsub_n_f32_x (p0, z1, -0.5),
		z0 = svsub_x (p0, z1, -0.5))

/*
** sub_2_f32_x_tied1:
**	fmov	(z[0-9]+\.s), #-2\.0(?:e\+0)?
**	fadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (sub_2_f32_x_tied1, svfloat32_t,
		z0 = svsub_n_f32_x (p0, z0, 2),
		z0 = svsub_x (p0, z0, 2))

/*
** sub_2_f32_x_untied:
**	fmov	z0\.s, #-2\.0(?:e\+0)?
**	fadd	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (sub_2_f32_x_untied, svfloat32_t,
		z0 = svsub_n_f32_x (p0, z1, 2),
		z0 = svsub_x (p0, z1, 2))

/*
** ptrue_sub_f32_x_tied1:
**	fsub	z0\.s, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (ptrue_sub_f32_x_tied1, svfloat32_t,
		z0 = svsub_f32_x (svptrue_b32 (), z0, z1),
		z0 = svsub_x (svptrue_b32 (), z0, z1))

/*
** ptrue_sub_f32_x_tied2:
**	fsub	z0\.s, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (ptrue_sub_f32_x_tied2, svfloat32_t,
		z0 = svsub_f32_x (svptrue_b32 (), z1, z0),
		z0 = svsub_x (svptrue_b32 (), z1, z0))

/*
** ptrue_sub_f32_x_untied:
**	fsub	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (ptrue_sub_f32_x_untied, svfloat32_t,
		z0 = svsub_f32_x (svptrue_b32 (), z1, z2),
		z0 = svsub_x (svptrue_b32 (), z1, z2))

/*
** ptrue_sub_1_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_sub_1_f32_x_tied1, svfloat32_t,
		z0 = svsub_n_f32_x (svptrue_b32 (), z0, 1),
		z0 = svsub_x (svptrue_b32 (), z0, 1))

/*
** ptrue_sub_1_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_sub_1_f32_x_untied, svfloat32_t,
		z0 = svsub_n_f32_x (svptrue_b32 (), z1, 1),
		z0 = svsub_x (svptrue_b32 (), z1, 1))

/*
** ptrue_sub_0p5_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_sub_0p5_f32_x_tied1, svfloat32_t,
		z0 = svsub_n_f32_x (svptrue_b32 (), z0, 0.5),
		z0 = svsub_x (svptrue_b32 (), z0, 0.5))

/*
** ptrue_sub_0p5_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_sub_0p5_f32_x_untied, svfloat32_t,
		z0 = svsub_n_f32_x (svptrue_b32 (), z1, 0.5),
		z0 = svsub_x (svptrue_b32 (), z1, 0.5))

/*
** ptrue_sub_m1_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_sub_m1_f32_x_tied1, svfloat32_t,
		z0 = svsub_n_f32_x (svptrue_b32 (), z0, -1),
		z0 = svsub_x (svptrue_b32 (), z0, -1))

/*
** ptrue_sub_m1_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_sub_m1_f32_x_untied, svfloat32_t,
		z0 = svsub_n_f32_x (svptrue_b32 (), z1, -1),
		z0 = svsub_x (svptrue_b32 (), z1, -1))

/*
** ptrue_sub_m0p5_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_sub_m0p5_f32_x_tied1, svfloat32_t,
		z0 = svsub_n_f32_x (svptrue_b32 (), z0, -0.5),
		z0 = svsub_x (svptrue_b32 (), z0, -0.5))

/*
** ptrue_sub_m0p5_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_sub_m0p5_f32_x_untied, svfloat32_t,
		z0 = svsub_n_f32_x (svptrue_b32 (), z1, -0.5),
		z0 = svsub_x (svptrue_b32 (), z1, -0.5))

/*
** ptrue_sub_2_f32_x_tied1:
**	fmov	(z[0-9]+\.s), #-2\.0(?:e\+0)?
**	fadd	z0\.s, (z0\.s, \1|\1, z0\.s)
**	ret
*/
TEST_UNIFORM_Z (ptrue_sub_2_f32_x_tied1, svfloat32_t,
		z0 = svsub_n_f32_x (svptrue_b32 (), z0, 2),
		z0 = svsub_x (svptrue_b32 (), z0, 2))

/*
** ptrue_sub_2_f32_x_untied:
**	fmov	(z[0-9]+\.s), #-2\.0(?:e\+0)?
**	fadd	z0\.s, (z1\.s, \1|\1, z1\.s)
**	ret
*/
TEST_UNIFORM_Z (ptrue_sub_2_f32_x_untied, svfloat32_t,
		z0 = svsub_n_f32_x (svptrue_b32 (), z1, 2),
		z0 = svsub_x (svptrue_b32 (), z1, 2))
