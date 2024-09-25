/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve+faminmax"

/*
** amax_f32_m_tied1:
**	famax	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (amax_f32_m_tied1, svfloat32_t,
		z0 = svamax_f32_m (p0, z0, z1),
		z0 = svamax_m (p0, z0, z1))

/*
** amax_f32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	...
**	famax	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (amax_f32_m_tied2, svfloat32_t,
		z0 = svamax_f32_m (p0, z1, z0),
		z0 = svamax_m (p0, z1, z0))

/*
** amax_f32_m_untied:
**	...
**	famax	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (amax_f32_m_untied, svfloat32_t,
		z0 = svamax_f32_m (p0, z1, z2),
		z0 = svamax_m (p0, z1, z2))

/*
** amax_s4_f32_m_tied1:
**	mov	(z[0-9]+\.s), s4
**	famax	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (amax_s4_f32_m_tied1, svfloat32_t, float,
		 z0 = svamax_n_f32_m (p0, z0, d4),
		 z0 = svamax_m (p0, z0, d4))

/*
** amax_s4_f32_m_untied:
**	mov	(z[0-9]+\.s), s4
**	...
**	famax	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (amax_s4_f32_m_untied, svfloat32_t, float,
		 z0 = svamax_n_f32_m (p0, z1, d4),
		 z0 = svamax_m (p0, z1, d4))

/*
** amax_0_f32_m_tied1:
**	...
**	famax	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amax_0_f32_m_tied1, svfloat32_t,
		z0 = svamax_n_f32_m (p0, z0, 0),
		z0 = svamax_m (p0, z0, 0))

/*
** amax_0_f32_m_untied:
**	...
**	famax	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amax_0_f32_m_untied, svfloat32_t,
		z0 = svamax_n_f32_m (p0, z1, 0),
		z0 = svamax_m (p0, z1, 0))

/*
** amax_1_f32_m_tied1:
**	...
**	famax	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amax_1_f32_m_tied1, svfloat32_t,
		z0 = svamax_n_f32_m (p0, z0, 1),
		z0 = svamax_m (p0, z0, 1))

/*
** amax_1_f32_m_untied:
**	...
**	famax	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amax_1_f32_m_untied, svfloat32_t,
		z0 = svamax_n_f32_m (p0, z1, 1),
		z0 = svamax_m (p0, z1, 1))

/*
** amax_2_f32_m:
**	...
**	famax	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amax_2_f32_m, svfloat32_t,
		z0 = svamax_n_f32_m (p0, z0, 2),
		z0 = svamax_m (p0, z0, 2))

/*
** amax_f32_z_tied1:
**	...
**	famax	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (amax_f32_z_tied1, svfloat32_t,
		z0 = svamax_f32_z (p0, z0, z1),
		z0 = svamax_z (p0, z0, z1))

/*
** amax_f32_z_tied2:
**	...
**	famax	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (amax_f32_z_tied2, svfloat32_t,
		z0 = svamax_f32_z (p0, z1, z0),
		z0 = svamax_z (p0, z1, z0))

/*
** amax_f32_z_untied:
** (
**	...
**	famax	z0\.s, p0/m, z0\.s, z2\.s
** |
**	...
**	famax	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (amax_f32_z_untied, svfloat32_t,
		z0 = svamax_f32_z (p0, z1, z2),
		z0 = svamax_z (p0, z1, z2))

/*
** amax_s4_f32_z_tied1:
**	mov	(z[0-9]+\.s), s4
**	...
**	famax	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (amax_s4_f32_z_tied1, svfloat32_t, float,
		 z0 = svamax_n_f32_z (p0, z0, d4),
		 z0 = svamax_z (p0, z0, d4))

/*
** amax_s4_f32_z_untied:
**	mov	(z[0-9]+\.s), s4
** (
**	...
**	famax	z0\.s, p0/m, z0\.s, \1
** |
**	...
**	famax	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZD (amax_s4_f32_z_untied, svfloat32_t, float,
		 z0 = svamax_n_f32_z (p0, z1, d4),
		 z0 = svamax_z (p0, z1, d4))

/*
** amax_0_f32_z_tied1:
**	...
**	famax	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amax_0_f32_z_tied1, svfloat32_t,
		z0 = svamax_n_f32_z (p0, z0, 0),
		z0 = svamax_z (p0, z0, 0))

/*
** amax_0_f32_z_untied:
**	...
**	famax	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amax_0_f32_z_untied, svfloat32_t,
		z0 = svamax_n_f32_z (p0, z1, 0),
		z0 = svamax_z (p0, z1, 0))

/*
** amax_1_f32_z_tied1:
**	...
**	famax	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amax_1_f32_z_tied1, svfloat32_t,
		z0 = svamax_n_f32_z (p0, z0, 1),
		z0 = svamax_z (p0, z0, 1))

/*
** amax_1_f32_z_untied:
**	...
**	famax	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amax_1_f32_z_untied, svfloat32_t,
		z0 = svamax_n_f32_z (p0, z1, 1),
		z0 = svamax_z (p0, z1, 1))

/*
** amax_2_f32_z:
**	...
**	famax	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amax_2_f32_z, svfloat32_t,
		z0 = svamax_n_f32_z (p0, z0, 2),
		z0 = svamax_z (p0, z0, 2))

/*
** amax_f32_x_tied1:
**	famax	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (amax_f32_x_tied1, svfloat32_t,
		z0 = svamax_f32_x (p0, z0, z1),
		z0 = svamax_x (p0, z0, z1))

/*
** amax_f32_x_tied2:
**	famax	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (amax_f32_x_tied2, svfloat32_t,
		z0 = svamax_f32_x (p0, z1, z0),
		z0 = svamax_x (p0, z1, z0))

/*
** amax_f32_x_untied:
** (
**	...
**	famax	z0\.s, p0/m, z0\.s, z2\.s
** |
**	...
**	famax	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (amax_f32_x_untied, svfloat32_t,
		z0 = svamax_f32_x (p0, z1, z2),
		z0 = svamax_x (p0, z1, z2))

/*
** amax_s4_f32_x_tied1:
**	mov	(z[0-9]+\.s), s4
**	famax	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (amax_s4_f32_x_tied1, svfloat32_t, float,
		 z0 = svamax_n_f32_x (p0, z0, d4),
		 z0 = svamax_x (p0, z0, d4))

/*
** amax_s4_f32_x_untied:
**	mov	z0\.s, s4
**	famax	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZD (amax_s4_f32_x_untied, svfloat32_t, float,
		 z0 = svamax_n_f32_x (p0, z1, d4),
		 z0 = svamax_x (p0, z1, d4))

/*
** amax_0_f32_x_tied1:
**	...
**	famax	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amax_0_f32_x_tied1, svfloat32_t,
		z0 = svamax_n_f32_x (p0, z0, 0),
		z0 = svamax_x (p0, z0, 0))

/*
** amax_0_f32_x_untied:
**	...
**	famax	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amax_0_f32_x_untied, svfloat32_t,
		z0 = svamax_n_f32_x (p0, z1, 0),
		z0 = svamax_x (p0, z1, 0))

/*
** amax_1_f32_x_tied1:
**	...
**	famax	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amax_1_f32_x_tied1, svfloat32_t,
		z0 = svamax_n_f32_x (p0, z0, 1),
		z0 = svamax_x (p0, z0, 1))

/*
** amax_1_f32_x_untied:
**	...
**	famax	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amax_1_f32_x_untied, svfloat32_t,
		z0 = svamax_n_f32_x (p0, z1, 1),
		z0 = svamax_x (p0, z1, 1))

/*
** amax_2_f32_x_tied1:
**	...
**	famax	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amax_2_f32_x_tied1, svfloat32_t,
		z0 = svamax_n_f32_x (p0, z0, 2),
		z0 = svamax_x (p0, z0, 2))

/*
** amax_2_f32_x_untied:
**	...
**	famax	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amax_2_f32_x_untied, svfloat32_t,
		z0 = svamax_n_f32_x (p0, z1, 2),
		z0 = svamax_x (p0, z1, 2))

/*
** ptrue_amax_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_f32_x_tied1, svfloat32_t,
		z0 = svamax_f32_x (svptrue_b32 (), z0, z1),
		z0 = svamax_x (svptrue_b32 (), z0, z1))

/*
** ptrue_amax_f32_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_f32_x_tied2, svfloat32_t,
		z0 = svamax_f32_x (svptrue_b32 (), z1, z0),
		z0 = svamax_x (svptrue_b32 (), z1, z0))

/*
** ptrue_amax_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_f32_x_untied, svfloat32_t,
		z0 = svamax_f32_x (svptrue_b32 (), z1, z2),
		z0 = svamax_x (svptrue_b32 (), z1, z2))

/*
** ptrue_amax_0_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_0_f32_x_tied1, svfloat32_t,
		z0 = svamax_n_f32_x (svptrue_b32 (), z0, 0),
		z0 = svamax_x (svptrue_b32 (), z0, 0))

/*
** ptrue_amax_0_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_0_f32_x_untied, svfloat32_t,
		z0 = svamax_n_f32_x (svptrue_b32 (), z1, 0),
		z0 = svamax_x (svptrue_b32 (), z1, 0))

/*
** ptrue_amax_1_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_1_f32_x_tied1, svfloat32_t,
		z0 = svamax_n_f32_x (svptrue_b32 (), z0, 1),
		z0 = svamax_x (svptrue_b32 (), z0, 1))

/*
** ptrue_amax_1_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_1_f32_x_untied, svfloat32_t,
		z0 = svamax_n_f32_x (svptrue_b32 (), z1, 1),
		z0 = svamax_x (svptrue_b32 (), z1, 1))

/*
** ptrue_amax_2_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_2_f32_x_tied1, svfloat32_t,
		z0 = svamax_n_f32_x (svptrue_b32 (), z0, 2),
		z0 = svamax_x (svptrue_b32 (), z0, 2))

/*
** ptrue_amax_2_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_2_f32_x_untied, svfloat32_t,
		z0 = svamax_n_f32_x (svptrue_b32 (), z1, 2),
		z0 = svamax_x (svptrue_b32 (), z1, 2))
