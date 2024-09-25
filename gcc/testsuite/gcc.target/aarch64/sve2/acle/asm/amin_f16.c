/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve+faminmax"

/*
** amin_f16_m_tied1:
**	famin	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (amin_f16_m_tied1, svfloat16_t,
		z0 = svamin_f16_m (p0, z0, z1),
		z0 = svamin_m (p0, z0, z1))

/*
** amin_f16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	...
**	famin	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (amin_f16_m_tied2, svfloat16_t,
		z0 = svamin_f16_m (p0, z1, z0),
		z0 = svamin_m (p0, z1, z0))

/*
** amin_f16_m_untied:
**	...
**	famin	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (amin_f16_m_untied, svfloat16_t,
		z0 = svamin_f16_m (p0, z1, z2),
		z0 = svamin_m (p0, z1, z2))

/*
** amin_h4_f16_m_tied1:
**	mov	(z[0-9]+\.h), h4
**	famin	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (amin_h4_f16_m_tied1, svfloat16_t, __fp16,
		 z0 = svamin_n_f16_m (p0, z0, d4),
		 z0 = svamin_m (p0, z0, d4))

/*
** amin_h4_f16_m_untied:
**	mov	(z[0-9]+\.h), h4
**	...
**	famin	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (amin_h4_f16_m_untied, svfloat16_t, __fp16,
		 z0 = svamin_n_f16_m (p0, z1, d4),
		 z0 = svamin_m (p0, z1, d4))

/*
** amin_0_f16_m_tied1:
**	...
**	famin	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amin_0_f16_m_tied1, svfloat16_t,
		z0 = svamin_n_f16_m (p0, z0, 0),
		z0 = svamin_m (p0, z0, 0))

/*
** amin_0_f16_m_untied:
**	...
**	famin	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amin_0_f16_m_untied, svfloat16_t,
		z0 = svamin_n_f16_m (p0, z1, 0),
		z0 = svamin_m (p0, z1, 0))

/*
** amin_1_f16_m_tied1:
**	...
**	famin	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amin_1_f16_m_tied1, svfloat16_t,
		z0 = svamin_n_f16_m (p0, z0, 1),
		z0 = svamin_m (p0, z0, 1))

/*
** amin_1_f16_m_untied:
**	...
**	famin	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amin_1_f16_m_untied, svfloat16_t,
		z0 = svamin_n_f16_m (p0, z1, 1),
		z0 = svamin_m (p0, z1, 1))

/*
** amin_2_f16_m:
**	...
**	famin	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amin_2_f16_m, svfloat16_t,
		z0 = svamin_n_f16_m (p0, z0, 2),
		z0 = svamin_m (p0, z0, 2))

/*
** amin_f16_z_tied1:
**	...
**	famin	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (amin_f16_z_tied1, svfloat16_t,
		z0 = svamin_f16_z (p0, z0, z1),
		z0 = svamin_z (p0, z0, z1))

/*
** amin_f16_z_tied2:
**	...
**	famin	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (amin_f16_z_tied2, svfloat16_t,
		z0 = svamin_f16_z (p0, z1, z0),
		z0 = svamin_z (p0, z1, z0))

/*
** amin_f16_z_untied:
** (
**	...
**	famin	z0\.h, p0/m, z0\.h, z2\.h
** |
**	...
**	famin	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (amin_f16_z_untied, svfloat16_t,
		z0 = svamin_f16_z (p0, z1, z2),
		z0 = svamin_z (p0, z1, z2))

/*
** amin_h4_f16_z_tied1:
**	mov	(z[0-9]+\.h), h4
**	...
**	famin	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (amin_h4_f16_z_tied1, svfloat16_t, __fp16,
		 z0 = svamin_n_f16_z (p0, z0, d4),
		 z0 = svamin_z (p0, z0, d4))

/*
** amin_h4_f16_z_untied:
**	mov	(z[0-9]+\.h), h4
** (
**	...
**	famin	z0\.h, p0/m, z0\.h, \1
** |
**	...
**	famin	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZD (amin_h4_f16_z_untied, svfloat16_t, __fp16,
		 z0 = svamin_n_f16_z (p0, z1, d4),
		 z0 = svamin_z (p0, z1, d4))

/*
** amin_0_f16_z_tied1:
**	...
**	famin	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amin_0_f16_z_tied1, svfloat16_t,
		z0 = svamin_n_f16_z (p0, z0, 0),
		z0 = svamin_z (p0, z0, 0))

/*
** amin_0_f16_z_untied:
**	...
**	famin	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amin_0_f16_z_untied, svfloat16_t,
		z0 = svamin_n_f16_z (p0, z1, 0),
		z0 = svamin_z (p0, z1, 0))

/*
** amin_1_f16_z_tied1:
**	...
**	famin	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amin_1_f16_z_tied1, svfloat16_t,
		z0 = svamin_n_f16_z (p0, z0, 1),
		z0 = svamin_z (p0, z0, 1))

/*
** amin_1_f16_z_untied:
**	...
**	famin	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amin_1_f16_z_untied, svfloat16_t,
		z0 = svamin_n_f16_z (p0, z1, 1),
		z0 = svamin_z (p0, z1, 1))

/*
** amin_2_f16_z:
**	...
**	famin	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amin_2_f16_z, svfloat16_t,
		z0 = svamin_n_f16_z (p0, z0, 2),
		z0 = svamin_z (p0, z0, 2))

/*
** amin_f16_x_tied1:
**	famin	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (amin_f16_x_tied1, svfloat16_t,
		z0 = svamin_f16_x (p0, z0, z1),
		z0 = svamin_x (p0, z0, z1))

/*
** amin_f16_x_tied2:
**	famin	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (amin_f16_x_tied2, svfloat16_t,
		z0 = svamin_f16_x (p0, z1, z0),
		z0 = svamin_x (p0, z1, z0))

/*
** amin_f16_x_untied:
** (
**	...
**	famin	z0\.h, p0/m, z0\.h, z2\.h
** |
**	...
**	famin	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (amin_f16_x_untied, svfloat16_t,
		z0 = svamin_f16_x (p0, z1, z2),
		z0 = svamin_x (p0, z1, z2))

/*
** amin_h4_f16_x_tied1:
**	mov	(z[0-9]+\.h), h4
**	famin	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (amin_h4_f16_x_tied1, svfloat16_t, __fp16,
		 z0 = svamin_n_f16_x (p0, z0, d4),
		 z0 = svamin_x (p0, z0, d4))

/*
** amin_h4_f16_x_untied:
**	mov	z0\.h, h4
**	famin	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZD (amin_h4_f16_x_untied, svfloat16_t, __fp16,
		 z0 = svamin_n_f16_x (p0, z1, d4),
		 z0 = svamin_x (p0, z1, d4))

/*
** amin_0_f16_x_tied1:
**	...
**	famin	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amin_0_f16_x_tied1, svfloat16_t,
		z0 = svamin_n_f16_x (p0, z0, 0),
		z0 = svamin_x (p0, z0, 0))

/*
** amin_0_f16_x_untied:
**	...
**	famin	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amin_0_f16_x_untied, svfloat16_t,
		z0 = svamin_n_f16_x (p0, z1, 0),
		z0 = svamin_x (p0, z1, 0))

/*
** amin_1_f16_x_tied1:
**	...
**	famin	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amin_1_f16_x_tied1, svfloat16_t,
		z0 = svamin_n_f16_x (p0, z0, 1),
		z0 = svamin_x (p0, z0, 1))

/*
** amin_1_f16_x_untied:
**	...
**	famin	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amin_1_f16_x_untied, svfloat16_t,
		z0 = svamin_n_f16_x (p0, z1, 1),
		z0 = svamin_x (p0, z1, 1))

/*
** amin_2_f16_x_tied1:
**	...
**	famin	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amin_2_f16_x_tied1, svfloat16_t,
		z0 = svamin_n_f16_x (p0, z0, 2),
		z0 = svamin_x (p0, z0, 2))

/*
** amin_2_f16_x_untied:
**	...
**	famin	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amin_2_f16_x_untied, svfloat16_t,
		z0 = svamin_n_f16_x (p0, z1, 2),
		z0 = svamin_x (p0, z1, 2))

/*
** ptrue_amin_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_f16_x_tied1, svfloat16_t,
		z0 = svamin_f16_x (svptrue_b16 (), z0, z1),
		z0 = svamin_x (svptrue_b16 (), z0, z1))

/*
** ptrue_amin_f16_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_f16_x_tied2, svfloat16_t,
		z0 = svamin_f16_x (svptrue_b16 (), z1, z0),
		z0 = svamin_x (svptrue_b16 (), z1, z0))

/*
** ptrue_amin_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_f16_x_untied, svfloat16_t,
		z0 = svamin_f16_x (svptrue_b16 (), z1, z2),
		z0 = svamin_x (svptrue_b16 (), z1, z2))

/*
** ptrue_amin_0_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_0_f16_x_tied1, svfloat16_t,
		z0 = svamin_n_f16_x (svptrue_b16 (), z0, 0),
		z0 = svamin_x (svptrue_b16 (), z0, 0))

/*
** ptrue_amin_0_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_0_f16_x_untied, svfloat16_t,
		z0 = svamin_n_f16_x (svptrue_b16 (), z1, 0),
		z0 = svamin_x (svptrue_b16 (), z1, 0))

/*
** ptrue_amin_1_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_1_f16_x_tied1, svfloat16_t,
		z0 = svamin_n_f16_x (svptrue_b16 (), z0, 1),
		z0 = svamin_x (svptrue_b16 (), z0, 1))

/*
** ptrue_amin_1_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_1_f16_x_untied, svfloat16_t,
		z0 = svamin_n_f16_x (svptrue_b16 (), z1, 1),
		z0 = svamin_x (svptrue_b16 (), z1, 1))

/*
** ptrue_amin_2_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_2_f16_x_tied1, svfloat16_t,
		z0 = svamin_n_f16_x (svptrue_b16 (), z0, 2),
		z0 = svamin_x (svptrue_b16 (), z0, 2))

/*
** ptrue_amin_2_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_2_f16_x_untied, svfloat16_t,
		z0 = svamin_n_f16_x (svptrue_b16 (), z1, 2),
		z0 = svamin_x (svptrue_b16 (), z1, 2))
