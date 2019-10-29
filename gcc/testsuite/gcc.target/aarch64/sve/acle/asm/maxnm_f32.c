/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** maxnm_f32_m_tied1:
**	fmaxnm	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (maxnm_f32_m_tied1, svfloat32_t,
		z0 = svmaxnm_f32_m (p0, z0, z1),
		z0 = svmaxnm_m (p0, z0, z1))

/*
** maxnm_f32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fmaxnm	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (maxnm_f32_m_tied2, svfloat32_t,
		z0 = svmaxnm_f32_m (p0, z1, z0),
		z0 = svmaxnm_m (p0, z1, z0))

/*
** maxnm_f32_m_untied:
**	movprfx	z0, z1
**	fmaxnm	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (maxnm_f32_m_untied, svfloat32_t,
		z0 = svmaxnm_f32_m (p0, z1, z2),
		z0 = svmaxnm_m (p0, z1, z2))

/*
** maxnm_s4_f32_m_tied1:
**	mov	(z[0-9]+\.s), s4
**	fmaxnm	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (maxnm_s4_f32_m_tied1, svfloat32_t, float,
		 z0 = svmaxnm_n_f32_m (p0, z0, d4),
		 z0 = svmaxnm_m (p0, z0, d4))

/*
** maxnm_s4_f32_m_untied:
**	mov	(z[0-9]+\.s), s4
**	movprfx	z0, z1
**	fmaxnm	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (maxnm_s4_f32_m_untied, svfloat32_t, float,
		 z0 = svmaxnm_n_f32_m (p0, z1, d4),
		 z0 = svmaxnm_m (p0, z1, d4))

/*
** maxnm_0_f32_m_tied1:
**	fmaxnm	z0\.s, p0/m, z0\.s, #0\.0
**	ret
*/
TEST_UNIFORM_Z (maxnm_0_f32_m_tied1, svfloat32_t,
		z0 = svmaxnm_n_f32_m (p0, z0, 0),
		z0 = svmaxnm_m (p0, z0, 0))

/*
** maxnm_0_f32_m_untied:
**	movprfx	z0, z1
**	fmaxnm	z0\.s, p0/m, z0\.s, #0\.0
**	ret
*/
TEST_UNIFORM_Z (maxnm_0_f32_m_untied, svfloat32_t,
		z0 = svmaxnm_n_f32_m (p0, z1, 0),
		z0 = svmaxnm_m (p0, z1, 0))

/*
** maxnm_1_f32_m_tied1:
**	fmaxnm	z0\.s, p0/m, z0\.s, #1\.0
**	ret
*/
TEST_UNIFORM_Z (maxnm_1_f32_m_tied1, svfloat32_t,
		z0 = svmaxnm_n_f32_m (p0, z0, 1),
		z0 = svmaxnm_m (p0, z0, 1))

/*
** maxnm_1_f32_m_untied:
**	movprfx	z0, z1
**	fmaxnm	z0\.s, p0/m, z0\.s, #1\.0
**	ret
*/
TEST_UNIFORM_Z (maxnm_1_f32_m_untied, svfloat32_t,
		z0 = svmaxnm_n_f32_m (p0, z1, 1),
		z0 = svmaxnm_m (p0, z1, 1))

/*
** maxnm_2_f32_m:
**	fmov	(z[0-9]+\.s), #2\.0(?:e\+0)?
**	fmaxnm	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (maxnm_2_f32_m, svfloat32_t,
		z0 = svmaxnm_n_f32_m (p0, z0, 2),
		z0 = svmaxnm_m (p0, z0, 2))

/*
** maxnm_f32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	fmaxnm	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (maxnm_f32_z_tied1, svfloat32_t,
		z0 = svmaxnm_f32_z (p0, z0, z1),
		z0 = svmaxnm_z (p0, z0, z1))

/*
** maxnm_f32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	fmaxnm	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (maxnm_f32_z_tied2, svfloat32_t,
		z0 = svmaxnm_f32_z (p0, z1, z0),
		z0 = svmaxnm_z (p0, z1, z0))

/*
** maxnm_f32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	fmaxnm	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	fmaxnm	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (maxnm_f32_z_untied, svfloat32_t,
		z0 = svmaxnm_f32_z (p0, z1, z2),
		z0 = svmaxnm_z (p0, z1, z2))

/*
** maxnm_s4_f32_z_tied1:
**	mov	(z[0-9]+\.s), s4
**	movprfx	z0\.s, p0/z, z0\.s
**	fmaxnm	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (maxnm_s4_f32_z_tied1, svfloat32_t, float,
		 z0 = svmaxnm_n_f32_z (p0, z0, d4),
		 z0 = svmaxnm_z (p0, z0, d4))

/*
** maxnm_s4_f32_z_untied:
**	mov	(z[0-9]+\.s), s4
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	fmaxnm	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	fmaxnm	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZD (maxnm_s4_f32_z_untied, svfloat32_t, float,
		 z0 = svmaxnm_n_f32_z (p0, z1, d4),
		 z0 = svmaxnm_z (p0, z1, d4))

/*
** maxnm_0_f32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	fmaxnm	z0\.s, p0/m, z0\.s, #0\.0
**	ret
*/
TEST_UNIFORM_Z (maxnm_0_f32_z_tied1, svfloat32_t,
		z0 = svmaxnm_n_f32_z (p0, z0, 0),
		z0 = svmaxnm_z (p0, z0, 0))

/*
** maxnm_0_f32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	fmaxnm	z0\.s, p0/m, z0\.s, #0\.0
**	ret
*/
TEST_UNIFORM_Z (maxnm_0_f32_z_untied, svfloat32_t,
		z0 = svmaxnm_n_f32_z (p0, z1, 0),
		z0 = svmaxnm_z (p0, z1, 0))

/*
** maxnm_1_f32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	fmaxnm	z0\.s, p0/m, z0\.s, #1\.0
**	ret
*/
TEST_UNIFORM_Z (maxnm_1_f32_z_tied1, svfloat32_t,
		z0 = svmaxnm_n_f32_z (p0, z0, 1),
		z0 = svmaxnm_z (p0, z0, 1))

/*
** maxnm_1_f32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	fmaxnm	z0\.s, p0/m, z0\.s, #1\.0
**	ret
*/
TEST_UNIFORM_Z (maxnm_1_f32_z_untied, svfloat32_t,
		z0 = svmaxnm_n_f32_z (p0, z1, 1),
		z0 = svmaxnm_z (p0, z1, 1))

/*
** maxnm_2_f32_z:
**	fmov	(z[0-9]+\.s), #2\.0(?:e\+0)?
**	movprfx	z0\.s, p0/z, z0\.s
**	fmaxnm	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (maxnm_2_f32_z, svfloat32_t,
		z0 = svmaxnm_n_f32_z (p0, z0, 2),
		z0 = svmaxnm_z (p0, z0, 2))

/*
** maxnm_f32_x_tied1:
**	fmaxnm	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (maxnm_f32_x_tied1, svfloat32_t,
		z0 = svmaxnm_f32_x (p0, z0, z1),
		z0 = svmaxnm_x (p0, z0, z1))

/*
** maxnm_f32_x_tied2:
**	fmaxnm	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (maxnm_f32_x_tied2, svfloat32_t,
		z0 = svmaxnm_f32_x (p0, z1, z0),
		z0 = svmaxnm_x (p0, z1, z0))

/*
** maxnm_f32_x_untied:
** (
**	movprfx	z0, z1
**	fmaxnm	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0, z2
**	fmaxnm	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (maxnm_f32_x_untied, svfloat32_t,
		z0 = svmaxnm_f32_x (p0, z1, z2),
		z0 = svmaxnm_x (p0, z1, z2))

/*
** maxnm_s4_f32_x_tied1:
**	mov	(z[0-9]+\.s), s4
**	fmaxnm	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (maxnm_s4_f32_x_tied1, svfloat32_t, float,
		 z0 = svmaxnm_n_f32_x (p0, z0, d4),
		 z0 = svmaxnm_x (p0, z0, d4))

/*
** maxnm_s4_f32_x_untied:
**	mov	z0\.s, s4
**	fmaxnm	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZD (maxnm_s4_f32_x_untied, svfloat32_t, float,
		 z0 = svmaxnm_n_f32_x (p0, z1, d4),
		 z0 = svmaxnm_x (p0, z1, d4))

/*
** maxnm_0_f32_x_tied1:
**	fmaxnm	z0\.s, p0/m, z0\.s, #0\.0
**	ret
*/
TEST_UNIFORM_Z (maxnm_0_f32_x_tied1, svfloat32_t,
		z0 = svmaxnm_n_f32_x (p0, z0, 0),
		z0 = svmaxnm_x (p0, z0, 0))

/*
** maxnm_0_f32_x_untied:
**	movprfx	z0, z1
**	fmaxnm	z0\.s, p0/m, z0\.s, #0\.0
**	ret
*/
TEST_UNIFORM_Z (maxnm_0_f32_x_untied, svfloat32_t,
		z0 = svmaxnm_n_f32_x (p0, z1, 0),
		z0 = svmaxnm_x (p0, z1, 0))

/*
** maxnm_1_f32_x_tied1:
**	fmaxnm	z0\.s, p0/m, z0\.s, #1\.0
**	ret
*/
TEST_UNIFORM_Z (maxnm_1_f32_x_tied1, svfloat32_t,
		z0 = svmaxnm_n_f32_x (p0, z0, 1),
		z0 = svmaxnm_x (p0, z0, 1))

/*
** maxnm_1_f32_x_untied:
**	movprfx	z0, z1
**	fmaxnm	z0\.s, p0/m, z0\.s, #1\.0
**	ret
*/
TEST_UNIFORM_Z (maxnm_1_f32_x_untied, svfloat32_t,
		z0 = svmaxnm_n_f32_x (p0, z1, 1),
		z0 = svmaxnm_x (p0, z1, 1))

/*
** maxnm_2_f32_x_tied1:
**	fmov	(z[0-9]+\.s), #2\.0(?:e\+0)?
**	fmaxnm	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (maxnm_2_f32_x_tied1, svfloat32_t,
		z0 = svmaxnm_n_f32_x (p0, z0, 2),
		z0 = svmaxnm_x (p0, z0, 2))

/*
** maxnm_2_f32_x_untied:
**	fmov	z0\.s, #2\.0(?:e\+0)?
**	fmaxnm	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (maxnm_2_f32_x_untied, svfloat32_t,
		z0 = svmaxnm_n_f32_x (p0, z1, 2),
		z0 = svmaxnm_x (p0, z1, 2))

/*
** ptrue_maxnm_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_maxnm_f32_x_tied1, svfloat32_t,
		z0 = svmaxnm_f32_x (svptrue_b32 (), z0, z1),
		z0 = svmaxnm_x (svptrue_b32 (), z0, z1))

/*
** ptrue_maxnm_f32_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_maxnm_f32_x_tied2, svfloat32_t,
		z0 = svmaxnm_f32_x (svptrue_b32 (), z1, z0),
		z0 = svmaxnm_x (svptrue_b32 (), z1, z0))

/*
** ptrue_maxnm_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_maxnm_f32_x_untied, svfloat32_t,
		z0 = svmaxnm_f32_x (svptrue_b32 (), z1, z2),
		z0 = svmaxnm_x (svptrue_b32 (), z1, z2))

/*
** ptrue_maxnm_0_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_maxnm_0_f32_x_tied1, svfloat32_t,
		z0 = svmaxnm_n_f32_x (svptrue_b32 (), z0, 0),
		z0 = svmaxnm_x (svptrue_b32 (), z0, 0))

/*
** ptrue_maxnm_0_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_maxnm_0_f32_x_untied, svfloat32_t,
		z0 = svmaxnm_n_f32_x (svptrue_b32 (), z1, 0),
		z0 = svmaxnm_x (svptrue_b32 (), z1, 0))

/*
** ptrue_maxnm_1_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_maxnm_1_f32_x_tied1, svfloat32_t,
		z0 = svmaxnm_n_f32_x (svptrue_b32 (), z0, 1),
		z0 = svmaxnm_x (svptrue_b32 (), z0, 1))

/*
** ptrue_maxnm_1_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_maxnm_1_f32_x_untied, svfloat32_t,
		z0 = svmaxnm_n_f32_x (svptrue_b32 (), z1, 1),
		z0 = svmaxnm_x (svptrue_b32 (), z1, 1))

/*
** ptrue_maxnm_2_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_maxnm_2_f32_x_tied1, svfloat32_t,
		z0 = svmaxnm_n_f32_x (svptrue_b32 (), z0, 2),
		z0 = svmaxnm_x (svptrue_b32 (), z0, 2))

/*
** ptrue_maxnm_2_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_maxnm_2_f32_x_untied, svfloat32_t,
		z0 = svmaxnm_n_f32_x (svptrue_b32 (), z1, 2),
		z0 = svmaxnm_x (svptrue_b32 (), z1, 2))
