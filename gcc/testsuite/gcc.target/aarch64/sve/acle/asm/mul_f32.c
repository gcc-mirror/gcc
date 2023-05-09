/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mul_f32_m_tied1:
**	fmul	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mul_f32_m_tied1, svfloat32_t,
		z0 = svmul_f32_m (p0, z0, z1),
		z0 = svmul_m (p0, z0, z1))

/*
** mul_f32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fmul	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (mul_f32_m_tied2, svfloat32_t,
		z0 = svmul_f32_m (p0, z1, z0),
		z0 = svmul_m (p0, z1, z0))

/*
** mul_f32_m_untied:
**	movprfx	z0, z1
**	fmul	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (mul_f32_m_untied, svfloat32_t,
		z0 = svmul_f32_m (p0, z1, z2),
		z0 = svmul_m (p0, z1, z2))

/*
** mul_s4_f32_m_tied1:
**	mov	(z[0-9]+\.s), s4
**	fmul	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (mul_s4_f32_m_tied1, svfloat32_t, float,
		 z0 = svmul_n_f32_m (p0, z0, d4),
		 z0 = svmul_m (p0, z0, d4))

/*
** mul_s4_f32_m_untied:
**	mov	(z[0-9]+\.s), s4
**	movprfx	z0, z1
**	fmul	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (mul_s4_f32_m_untied, svfloat32_t, float,
		 z0 = svmul_n_f32_m (p0, z1, d4),
		 z0 = svmul_m (p0, z1, d4))

/*
** mul_1_f32_m_tied1:
**	fmov	(z[0-9]+\.s), #1\.0(?:e\+0)?
**	fmul	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (mul_1_f32_m_tied1, svfloat32_t,
		z0 = svmul_n_f32_m (p0, z0, 1),
		z0 = svmul_m (p0, z0, 1))

/*
** mul_1_f32_m_untied:
**	fmov	(z[0-9]+\.s), #1\.0(?:e\+0)?
**	movprfx	z0, z1
**	fmul	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (mul_1_f32_m_untied, svfloat32_t,
		z0 = svmul_n_f32_m (p0, z1, 1),
		z0 = svmul_m (p0, z1, 1))

/*
** mul_0p5_f32_m_tied1:
**	fmul	z0\.s, p0/m, z0\.s, #0\.5
**	ret
*/
TEST_UNIFORM_Z (mul_0p5_f32_m_tied1, svfloat32_t,
		z0 = svmul_n_f32_m (p0, z0, 0.5),
		z0 = svmul_m (p0, z0, 0.5))

/*
** mul_0p5_f32_m_untied:
**	movprfx	z0, z1
**	fmul	z0\.s, p0/m, z0\.s, #0\.5
**	ret
*/
TEST_UNIFORM_Z (mul_0p5_f32_m_untied, svfloat32_t,
		z0 = svmul_n_f32_m (p0, z1, 0.5),
		z0 = svmul_m (p0, z1, 0.5))

/*
** mul_2_f32_m_tied1:
**	fmul	z0\.s, p0/m, z0\.s, #2\.0
**	ret
*/
TEST_UNIFORM_Z (mul_2_f32_m_tied1, svfloat32_t,
		z0 = svmul_n_f32_m (p0, z0, 2),
		z0 = svmul_m (p0, z0, 2))

/*
** mul_2_f32_m_untied:
**	movprfx	z0, z1
**	fmul	z0\.s, p0/m, z0\.s, #2\.0
**	ret
*/
TEST_UNIFORM_Z (mul_2_f32_m_untied, svfloat32_t,
		z0 = svmul_n_f32_m (p0, z1, 2),
		z0 = svmul_m (p0, z1, 2))

/*
** mul_f32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	fmul	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mul_f32_z_tied1, svfloat32_t,
		z0 = svmul_f32_z (p0, z0, z1),
		z0 = svmul_z (p0, z0, z1))

/*
** mul_f32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	fmul	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mul_f32_z_tied2, svfloat32_t,
		z0 = svmul_f32_z (p0, z1, z0),
		z0 = svmul_z (p0, z1, z0))

/*
** mul_f32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	fmul	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	fmul	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (mul_f32_z_untied, svfloat32_t,
		z0 = svmul_f32_z (p0, z1, z2),
		z0 = svmul_z (p0, z1, z2))

/*
** mul_s4_f32_z_tied1:
**	mov	(z[0-9]+\.s), s4
**	movprfx	z0\.s, p0/z, z0\.s
**	fmul	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (mul_s4_f32_z_tied1, svfloat32_t, float,
		 z0 = svmul_n_f32_z (p0, z0, d4),
		 z0 = svmul_z (p0, z0, d4))

/*
** mul_s4_f32_z_untied:
**	mov	(z[0-9]+\.s), s4
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	fmul	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	fmul	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZD (mul_s4_f32_z_untied, svfloat32_t, float,
		 z0 = svmul_n_f32_z (p0, z1, d4),
		 z0 = svmul_z (p0, z1, d4))

/*
** mul_1_f32_z_tied1:
**	fmov	(z[0-9]+\.s), #1\.0(?:e\+0)?
**	movprfx	z0\.s, p0/z, z0\.s
**	fmul	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (mul_1_f32_z_tied1, svfloat32_t,
		z0 = svmul_n_f32_z (p0, z0, 1),
		z0 = svmul_z (p0, z0, 1))

/*
** mul_1_f32_z_untied:
**	fmov	(z[0-9]+\.s), #1\.0(?:e\+0)?
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	fmul	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	fmul	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (mul_1_f32_z_untied, svfloat32_t,
		z0 = svmul_n_f32_z (p0, z1, 1),
		z0 = svmul_z (p0, z1, 1))

/*
** mul_0p5_f32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	fmul	z0\.s, p0/m, z0\.s, #0\.5
**	ret
*/
TEST_UNIFORM_Z (mul_0p5_f32_z_tied1, svfloat32_t,
		z0 = svmul_n_f32_z (p0, z0, 0.5),
		z0 = svmul_z (p0, z0, 0.5))

/*
** mul_0p5_f32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	fmul	z0\.s, p0/m, z0\.s, #0\.5
**	ret
*/
TEST_UNIFORM_Z (mul_0p5_f32_z_untied, svfloat32_t,
		z0 = svmul_n_f32_z (p0, z1, 0.5),
		z0 = svmul_z (p0, z1, 0.5))

/*
** mul_2_f32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	fmul	z0\.s, p0/m, z0\.s, #2\.0
**	ret
*/
TEST_UNIFORM_Z (mul_2_f32_z_tied1, svfloat32_t,
		z0 = svmul_n_f32_z (p0, z0, 2),
		z0 = svmul_z (p0, z0, 2))

/*
** mul_2_f32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	fmul	z0\.s, p0/m, z0\.s, #2\.0
**	ret
*/
TEST_UNIFORM_Z (mul_2_f32_z_untied, svfloat32_t,
		z0 = svmul_n_f32_z (p0, z1, 2),
		z0 = svmul_z (p0, z1, 2))

/*
** mul_f32_x_tied1:
**	fmul	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mul_f32_x_tied1, svfloat32_t,
		z0 = svmul_f32_x (p0, z0, z1),
		z0 = svmul_x (p0, z0, z1))

/*
** mul_f32_x_tied2:
**	fmul	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mul_f32_x_tied2, svfloat32_t,
		z0 = svmul_f32_x (p0, z1, z0),
		z0 = svmul_x (p0, z1, z0))

/*
** mul_f32_x_untied:
** (
**	movprfx	z0, z1
**	fmul	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0, z2
**	fmul	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (mul_f32_x_untied, svfloat32_t,
		z0 = svmul_f32_x (p0, z1, z2),
		z0 = svmul_x (p0, z1, z2))

/*
** mul_s4_f32_x_tied1:
**	mov	(z[0-9]+\.s), s4
**	fmul	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (mul_s4_f32_x_tied1, svfloat32_t, float,
		 z0 = svmul_n_f32_x (p0, z0, d4),
		 z0 = svmul_x (p0, z0, d4))

/*
** mul_s4_f32_x_untied:
**	mov	z0\.s, s4
**	fmul	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZD (mul_s4_f32_x_untied, svfloat32_t, float,
		 z0 = svmul_n_f32_x (p0, z1, d4),
		 z0 = svmul_x (p0, z1, d4))

/*
** mul_1_f32_x_tied1:
**	fmov	(z[0-9]+\.s), #1\.0(?:e\+0)?
**	fmul	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (mul_1_f32_x_tied1, svfloat32_t,
		z0 = svmul_n_f32_x (p0, z0, 1),
		z0 = svmul_x (p0, z0, 1))

/*
** mul_1_f32_x_untied:
**	fmov	z0\.s, #1\.0(?:e\+0)?
**	fmul	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mul_1_f32_x_untied, svfloat32_t,
		z0 = svmul_n_f32_x (p0, z1, 1),
		z0 = svmul_x (p0, z1, 1))

/*
** mul_0p5_f32_x_tied1:
**	fmul	z0\.s, p0/m, z0\.s, #0\.5
**	ret
*/
TEST_UNIFORM_Z (mul_0p5_f32_x_tied1, svfloat32_t,
		z0 = svmul_n_f32_x (p0, z0, 0.5),
		z0 = svmul_x (p0, z0, 0.5))

/*
** mul_0p5_f32_x_untied:
**	movprfx	z0, z1
**	fmul	z0\.s, p0/m, z0\.s, #0\.5
**	ret
*/
TEST_UNIFORM_Z (mul_0p5_f32_x_untied, svfloat32_t,
		z0 = svmul_n_f32_x (p0, z1, 0.5),
		z0 = svmul_x (p0, z1, 0.5))

/*
** mul_2_f32_x_tied1:
**	fmul	z0\.s, p0/m, z0\.s, #2\.0
**	ret
*/
TEST_UNIFORM_Z (mul_2_f32_x_tied1, svfloat32_t,
		z0 = svmul_n_f32_x (p0, z0, 2),
		z0 = svmul_x (p0, z0, 2))

/*
** mul_2_f32_x_untied:
**	movprfx	z0, z1
**	fmul	z0\.s, p0/m, z0\.s, #2\.0
**	ret
*/
TEST_UNIFORM_Z (mul_2_f32_x_untied, svfloat32_t,
		z0 = svmul_n_f32_x (p0, z1, 2),
		z0 = svmul_x (p0, z1, 2))

/*
** ptrue_mul_f32_x_tied1:
**	fmul	z0\.s, (z0\.s, z1\.s|z1\.s, z0\.s)
**	ret
*/
TEST_UNIFORM_Z (ptrue_mul_f32_x_tied1, svfloat32_t,
		z0 = svmul_f32_x (svptrue_b32 (), z0, z1),
		z0 = svmul_x (svptrue_b32 (), z0, z1))

/*
** ptrue_mul_f32_x_tied2:
**	fmul	z0\.s, (z0\.s, z1\.s|z1\.s, z0\.s)
**	ret
*/
TEST_UNIFORM_Z (ptrue_mul_f32_x_tied2, svfloat32_t,
		z0 = svmul_f32_x (svptrue_b32 (), z1, z0),
		z0 = svmul_x (svptrue_b32 (), z1, z0))

/*
** ptrue_mul_f32_x_untied:
**	fmul	z0\.s, (z1\.s, z2\.s|z2\.s, z1\.s)
**	ret
*/
TEST_UNIFORM_Z (ptrue_mul_f32_x_untied, svfloat32_t,
		z0 = svmul_f32_x (svptrue_b32 (), z1, z2),
		z0 = svmul_x (svptrue_b32 (), z1, z2))

/*
** ptrue_mul_1_f32_x_tied1:
**	fmov	(z[0-9]+\.s), #1\.0(?:e\+0)?
**	fmul	z0\.s, (z0\.s, \1|\1, z0\.s)
**	ret
*/
TEST_UNIFORM_Z (ptrue_mul_1_f32_x_tied1, svfloat32_t,
		z0 = svmul_n_f32_x (svptrue_b32 (), z0, 1),
		z0 = svmul_x (svptrue_b32 (), z0, 1))

/*
** ptrue_mul_1_f32_x_untied:
**	fmov	(z[0-9]+\.s), #1\.0(?:e\+0)?
**	fmul	z0\.s, (z1\.s, \1|\1, z1\.s)
**	ret
*/
TEST_UNIFORM_Z (ptrue_mul_1_f32_x_untied, svfloat32_t,
		z0 = svmul_n_f32_x (svptrue_b32 (), z1, 1),
		z0 = svmul_x (svptrue_b32 (), z1, 1))

/*
** ptrue_mul_0p5_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_mul_0p5_f32_x_tied1, svfloat32_t,
		z0 = svmul_n_f32_x (svptrue_b32 (), z0, 0.5),
		z0 = svmul_x (svptrue_b32 (), z0, 0.5))

/*
** ptrue_mul_0p5_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_mul_0p5_f32_x_untied, svfloat32_t,
		z0 = svmul_n_f32_x (svptrue_b32 (), z1, 0.5),
		z0 = svmul_x (svptrue_b32 (), z1, 0.5))

/*
** ptrue_mul_2_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_mul_2_f32_x_tied1, svfloat32_t,
		z0 = svmul_n_f32_x (svptrue_b32 (), z0, 2),
		z0 = svmul_x (svptrue_b32 (), z0, 2))

/*
** ptrue_mul_2_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_mul_2_f32_x_untied, svfloat32_t,
		z0 = svmul_n_f32_x (svptrue_b32 (), z1, 2),
		z0 = svmul_x (svptrue_b32 (), z1, 2))
