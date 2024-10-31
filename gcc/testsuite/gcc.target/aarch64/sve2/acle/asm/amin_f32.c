/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2+faminmax"
#if STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** amin_f32_m_tied1:
**	famin	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (amin_f32_m_tied1, svfloat32_t,
		z0 = svamin_f32_m (p0, z0, z1),
		z0 = svamin_m (p0, z0, z1))

/*
** amin_f32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	...
**	famin	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (amin_f32_m_tied2, svfloat32_t,
		z0 = svamin_f32_m (p0, z1, z0),
		z0 = svamin_m (p0, z1, z0))

/*
** amin_f32_m_untied:
**	...
**	famin	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (amin_f32_m_untied, svfloat32_t,
		z0 = svamin_f32_m (p0, z1, z2),
		z0 = svamin_m (p0, z1, z2))

/*
** amin_s4_f32_m_tied1:
**	mov	(z[0-9]+\.s), s4
**	famin	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (amin_s4_f32_m_tied1, svfloat32_t, float,
		 z0 = svamin_n_f32_m (p0, z0, d4),
		 z0 = svamin_m (p0, z0, d4))

/*
** amin_s4_f32_m_untied:
**	mov	(z[0-9]+\.s), s4
**	...
**	famin	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (amin_s4_f32_m_untied, svfloat32_t, float,
		 z0 = svamin_n_f32_m (p0, z1, d4),
		 z0 = svamin_m (p0, z1, d4))

/*
** amin_0_f32_m_tied1:
**	...
**	famin	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amin_0_f32_m_tied1, svfloat32_t,
		z0 = svamin_n_f32_m (p0, z0, 0),
		z0 = svamin_m (p0, z0, 0))

/*
** amin_0_f32_m_untied:
**	...
**	famin	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amin_0_f32_m_untied, svfloat32_t,
		z0 = svamin_n_f32_m (p0, z1, 0),
		z0 = svamin_m (p0, z1, 0))

/*
** amin_1_f32_m_tied1:
**	...
**	famin	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amin_1_f32_m_tied1, svfloat32_t,
		z0 = svamin_n_f32_m (p0, z0, 1),
		z0 = svamin_m (p0, z0, 1))

/*
** amin_1_f32_m_untied:
**	...
**	famin	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amin_1_f32_m_untied, svfloat32_t,
		z0 = svamin_n_f32_m (p0, z1, 1),
		z0 = svamin_m (p0, z1, 1))

/*
** amin_2_f32_m:
**	...
**	famin	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amin_2_f32_m, svfloat32_t,
		z0 = svamin_n_f32_m (p0, z0, 2),
		z0 = svamin_m (p0, z0, 2))

/*
** amin_f32_z_tied1:
**	...
**	famin	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (amin_f32_z_tied1, svfloat32_t,
		z0 = svamin_f32_z (p0, z0, z1),
		z0 = svamin_z (p0, z0, z1))

/*
** amin_f32_z_tied2:
**	...
**	famin	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (amin_f32_z_tied2, svfloat32_t,
		z0 = svamin_f32_z (p0, z1, z0),
		z0 = svamin_z (p0, z1, z0))

/*
** amin_f32_z_untied:
** (
**	...
**	famin	z0\.s, p0/m, z0\.s, z2\.s
** |
**	...
**	famin	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (amin_f32_z_untied, svfloat32_t,
		z0 = svamin_f32_z (p0, z1, z2),
		z0 = svamin_z (p0, z1, z2))

/*
** amin_s4_f32_z_tied1:
**	mov	(z[0-9]+\.s), s4
**	...
**	famin	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (amin_s4_f32_z_tied1, svfloat32_t, float,
		 z0 = svamin_n_f32_z (p0, z0, d4),
		 z0 = svamin_z (p0, z0, d4))

/*
** amin_s4_f32_z_untied:
**	mov	(z[0-9]+\.s), s4
** (
**	...
**	famin	z0\.s, p0/m, z0\.s, \1
** |
**	...
**	famin	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZD (amin_s4_f32_z_untied, svfloat32_t, float,
		 z0 = svamin_n_f32_z (p0, z1, d4),
		 z0 = svamin_z (p0, z1, d4))

/*
** amin_0_f32_z_tied1:
**	...
**	famin	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amin_0_f32_z_tied1, svfloat32_t,
		z0 = svamin_n_f32_z (p0, z0, 0),
		z0 = svamin_z (p0, z0, 0))

/*
** amin_0_f32_z_untied:
**	...
**	famin	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amin_0_f32_z_untied, svfloat32_t,
		z0 = svamin_n_f32_z (p0, z1, 0),
		z0 = svamin_z (p0, z1, 0))

/*
** amin_1_f32_z_tied1:
**	...
**	famin	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amin_1_f32_z_tied1, svfloat32_t,
		z0 = svamin_n_f32_z (p0, z0, 1),
		z0 = svamin_z (p0, z0, 1))

/*
** amin_1_f32_z_untied:
**	...
**	famin	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amin_1_f32_z_untied, svfloat32_t,
		z0 = svamin_n_f32_z (p0, z1, 1),
		z0 = svamin_z (p0, z1, 1))

/*
** amin_2_f32_z:
**	...
**	famin	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amin_2_f32_z, svfloat32_t,
		z0 = svamin_n_f32_z (p0, z0, 2),
		z0 = svamin_z (p0, z0, 2))

/*
** amin_f32_x_tied1:
**	famin	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (amin_f32_x_tied1, svfloat32_t,
		z0 = svamin_f32_x (p0, z0, z1),
		z0 = svamin_x (p0, z0, z1))

/*
** amin_f32_x_tied2:
**	famin	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (amin_f32_x_tied2, svfloat32_t,
		z0 = svamin_f32_x (p0, z1, z0),
		z0 = svamin_x (p0, z1, z0))

/*
** amin_f32_x_untied:
** (
**	...
**	famin	z0\.s, p0/m, z0\.s, z2\.s
** |
**	...
**	famin	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (amin_f32_x_untied, svfloat32_t,
		z0 = svamin_f32_x (p0, z1, z2),
		z0 = svamin_x (p0, z1, z2))

/*
** amin_s4_f32_x_tied1:
**	mov	(z[0-9]+\.s), s4
**	famin	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZD (amin_s4_f32_x_tied1, svfloat32_t, float,
		 z0 = svamin_n_f32_x (p0, z0, d4),
		 z0 = svamin_x (p0, z0, d4))

/*
** amin_s4_f32_x_untied:
**	mov	z0\.s, s4
**	famin	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZD (amin_s4_f32_x_untied, svfloat32_t, float,
		 z0 = svamin_n_f32_x (p0, z1, d4),
		 z0 = svamin_x (p0, z1, d4))

/*
** amin_0_f32_x_tied1:
**	...
**	famin	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amin_0_f32_x_tied1, svfloat32_t,
		z0 = svamin_n_f32_x (p0, z0, 0),
		z0 = svamin_x (p0, z0, 0))

/*
** amin_0_f32_x_untied:
**	...
**	famin	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amin_0_f32_x_untied, svfloat32_t,
		z0 = svamin_n_f32_x (p0, z1, 0),
		z0 = svamin_x (p0, z1, 0))

/*
** amin_1_f32_x_tied1:
**	...
**	famin	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amin_1_f32_x_tied1, svfloat32_t,
		z0 = svamin_n_f32_x (p0, z0, 1),
		z0 = svamin_x (p0, z0, 1))

/*
** amin_1_f32_x_untied:
**	...
**	famin	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amin_1_f32_x_untied, svfloat32_t,
		z0 = svamin_n_f32_x (p0, z1, 1),
		z0 = svamin_x (p0, z1, 1))

/*
** amin_2_f32_x_tied1:
**	...
**	famin	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amin_2_f32_x_tied1, svfloat32_t,
		z0 = svamin_n_f32_x (p0, z0, 2),
		z0 = svamin_x (p0, z0, 2))

/*
** amin_2_f32_x_untied:
**	...
**	famin	z0\.s, p0/m, z0\.s, z[0-9]+\.s
**	ret
*/
TEST_UNIFORM_Z (amin_2_f32_x_untied, svfloat32_t,
		z0 = svamin_n_f32_x (p0, z1, 2),
		z0 = svamin_x (p0, z1, 2))

/*
** ptrue_amin_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_f32_x_tied1, svfloat32_t,
		z0 = svamin_f32_x (svptrue_b32 (), z0, z1),
		z0 = svamin_x (svptrue_b32 (), z0, z1))

/*
** ptrue_amin_f32_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_f32_x_tied2, svfloat32_t,
		z0 = svamin_f32_x (svptrue_b32 (), z1, z0),
		z0 = svamin_x (svptrue_b32 (), z1, z0))

/*
** ptrue_amin_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_f32_x_untied, svfloat32_t,
		z0 = svamin_f32_x (svptrue_b32 (), z1, z2),
		z0 = svamin_x (svptrue_b32 (), z1, z2))

/*
** ptrue_amin_0_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_0_f32_x_tied1, svfloat32_t,
		z0 = svamin_n_f32_x (svptrue_b32 (), z0, 0),
		z0 = svamin_x (svptrue_b32 (), z0, 0))

/*
** ptrue_amin_0_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_0_f32_x_untied, svfloat32_t,
		z0 = svamin_n_f32_x (svptrue_b32 (), z1, 0),
		z0 = svamin_x (svptrue_b32 (), z1, 0))

/*
** ptrue_amin_1_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_1_f32_x_tied1, svfloat32_t,
		z0 = svamin_n_f32_x (svptrue_b32 (), z0, 1),
		z0 = svamin_x (svptrue_b32 (), z0, 1))

/*
** ptrue_amin_1_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_1_f32_x_untied, svfloat32_t,
		z0 = svamin_n_f32_x (svptrue_b32 (), z1, 1),
		z0 = svamin_x (svptrue_b32 (), z1, 1))

/*
** ptrue_amin_2_f32_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_2_f32_x_tied1, svfloat32_t,
		z0 = svamin_n_f32_x (svptrue_b32 (), z0, 2),
		z0 = svamin_x (svptrue_b32 (), z0, 2))

/*
** ptrue_amin_2_f32_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_2_f32_x_untied, svfloat32_t,
		z0 = svamin_n_f32_x (svptrue_b32 (), z1, 2),
		z0 = svamin_x (svptrue_b32 (), z1, 2))
