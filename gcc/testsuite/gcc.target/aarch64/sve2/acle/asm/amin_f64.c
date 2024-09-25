/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve+faminmax"

/*
** amin_f64_m_tied1:
**	famin	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (amin_f64_m_tied1, svfloat64_t,
		z0 = svamin_f64_m (p0, z0, z1),
		z0 = svamin_m (p0, z0, z1))

/*
** amin_f64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	...
**	famin	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (amin_f64_m_tied2, svfloat64_t,
		z0 = svamin_f64_m (p0, z1, z0),
		z0 = svamin_m (p0, z1, z0))

/*
** amin_f64_m_untied:
**	...
**	famin	z0\.d, p0/m, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (amin_f64_m_untied, svfloat64_t,
		z0 = svamin_f64_m (p0, z1, z2),
		z0 = svamin_m (p0, z1, z2))

/*
** amin_d4_f64_m_tied1:
**	mov	(z[0-9]+\.d), d4
**	famin	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (amin_d4_f64_m_tied1, svfloat64_t, double,
		 z0 = svamin_n_f64_m (p0, z0, d4),
		 z0 = svamin_m (p0, z0, d4))

/*
** amin_d4_f64_m_untied:
**	mov	(z[0-9]+\.d), d4
**	...
**	famin	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (amin_d4_f64_m_untied, svfloat64_t, double,
		 z0 = svamin_n_f64_m (p0, z1, d4),
		 z0 = svamin_m (p0, z1, d4))

/*
** amin_0_f64_m_tied1:
**	...
**	famin	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amin_0_f64_m_tied1, svfloat64_t,
		z0 = svamin_n_f64_m (p0, z0, 0),
		z0 = svamin_m (p0, z0, 0))

/*
** amin_0_f64_m_untied:
**	...
**	famin	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amin_0_f64_m_untied, svfloat64_t,
		z0 = svamin_n_f64_m (p0, z1, 0),
		z0 = svamin_m (p0, z1, 0))

/*
** amin_1_f64_m_tied1:
**	...
**	famin	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amin_1_f64_m_tied1, svfloat64_t,
		z0 = svamin_n_f64_m (p0, z0, 1),
		z0 = svamin_m (p0, z0, 1))

/*
** amin_1_f64_m_untied:
**	...
**	famin	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amin_1_f64_m_untied, svfloat64_t,
		z0 = svamin_n_f64_m (p0, z1, 1),
		z0 = svamin_m (p0, z1, 1))

/*
** amin_2_f64_m:
**	...
**	famin	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amin_2_f64_m, svfloat64_t,
		z0 = svamin_n_f64_m (p0, z0, 2),
		z0 = svamin_m (p0, z0, 2))

/*
** amin_f64_z_tied1:
**	...
**	famin	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (amin_f64_z_tied1, svfloat64_t,
		z0 = svamin_f64_z (p0, z0, z1),
		z0 = svamin_z (p0, z0, z1))

/*
** amin_f64_z_tied2:
**	...
**	famin	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (amin_f64_z_tied2, svfloat64_t,
		z0 = svamin_f64_z (p0, z1, z0),
		z0 = svamin_z (p0, z1, z0))

/*
** amin_f64_z_untied:
** (
**	...
**	famin	z0\.d, p0/m, z0\.d, z2\.d
** |
**	...
**	famin	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (amin_f64_z_untied, svfloat64_t,
		z0 = svamin_f64_z (p0, z1, z2),
		z0 = svamin_z (p0, z1, z2))

/*
** amin_d4_f64_z_tied1:
**	mov	(z[0-9]+\.d), d4
**	...
**	famin	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (amin_d4_f64_z_tied1, svfloat64_t, double,
		 z0 = svamin_n_f64_z (p0, z0, d4),
		 z0 = svamin_z (p0, z0, d4))

/*
** amin_d4_f64_z_untied:
**	mov	(z[0-9]+\.d), d4
** (
**	...
**	famin	z0\.d, p0/m, z0\.d, \1
** |
**	...
**	famin	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZD (amin_d4_f64_z_untied, svfloat64_t, double,
		 z0 = svamin_n_f64_z (p0, z1, d4),
		 z0 = svamin_z (p0, z1, d4))

/*
** amin_0_f64_z_tied1:
**	...
**	famin	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amin_0_f64_z_tied1, svfloat64_t,
		z0 = svamin_n_f64_z (p0, z0, 0),
		z0 = svamin_z (p0, z0, 0))

/*
** amin_0_f64_z_untied:
**	...
**	famin	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amin_0_f64_z_untied, svfloat64_t,
		z0 = svamin_n_f64_z (p0, z1, 0),
		z0 = svamin_z (p0, z1, 0))

/*
** amin_1_f64_z_tied1:
**	...
**	famin	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amin_1_f64_z_tied1, svfloat64_t,
		z0 = svamin_n_f64_z (p0, z0, 1),
		z0 = svamin_z (p0, z0, 1))

/*
** amin_1_f64_z_untied:
**	...
**	famin	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amin_1_f64_z_untied, svfloat64_t,
		z0 = svamin_n_f64_z (p0, z1, 1),
		z0 = svamin_z (p0, z1, 1))

/*
** amin_2_f64_z:
**	...
**	famin	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amin_2_f64_z, svfloat64_t,
		z0 = svamin_n_f64_z (p0, z0, 2),
		z0 = svamin_z (p0, z0, 2))

/*
** amin_f64_x_tied1:
**	famin	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (amin_f64_x_tied1, svfloat64_t,
		z0 = svamin_f64_x (p0, z0, z1),
		z0 = svamin_x (p0, z0, z1))

/*
** amin_f64_x_tied2:
**	famin	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (amin_f64_x_tied2, svfloat64_t,
		z0 = svamin_f64_x (p0, z1, z0),
		z0 = svamin_x (p0, z1, z0))

/*
** amin_f64_x_untied:
** (
**	...
**	famin	z0\.d, p0/m, z0\.d, z2\.d
** |
**	...
**	famin	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (amin_f64_x_untied, svfloat64_t,
		z0 = svamin_f64_x (p0, z1, z2),
		z0 = svamin_x (p0, z1, z2))

/*
** amin_d4_f64_x_tied1:
**	mov	(z[0-9]+\.d), d4
**	famin	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (amin_d4_f64_x_tied1, svfloat64_t, double,
		 z0 = svamin_n_f64_x (p0, z0, d4),
		 z0 = svamin_x (p0, z0, d4))

/*
** amin_d4_f64_x_untied:
**	mov	z0\.d, d4
**	famin	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_ZD (amin_d4_f64_x_untied, svfloat64_t, double,
		 z0 = svamin_n_f64_x (p0, z1, d4),
		 z0 = svamin_x (p0, z1, d4))

/*
** amin_0_f64_x_tied1:
**	...
**	famin	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amin_0_f64_x_tied1, svfloat64_t,
		z0 = svamin_n_f64_x (p0, z0, 0),
		z0 = svamin_x (p0, z0, 0))

/*
** amin_0_f64_x_untied:
**	...
**	famin	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amin_0_f64_x_untied, svfloat64_t,
		z0 = svamin_n_f64_x (p0, z1, 0),
		z0 = svamin_x (p0, z1, 0))

/*
** amin_1_f64_x_tied1:
**	...
**	famin	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amin_1_f64_x_tied1, svfloat64_t,
		z0 = svamin_n_f64_x (p0, z0, 1),
		z0 = svamin_x (p0, z0, 1))

/*
** amin_1_f64_x_untied:
**	...
**	famin	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amin_1_f64_x_untied, svfloat64_t,
		z0 = svamin_n_f64_x (p0, z1, 1),
		z0 = svamin_x (p0, z1, 1))

/*
** amin_2_f64_x_tied1:
**	...
**	famin	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amin_2_f64_x_tied1, svfloat64_t,
		z0 = svamin_n_f64_x (p0, z0, 2),
		z0 = svamin_x (p0, z0, 2))

/*
** amin_2_f64_x_untied:
**	...
**	famin	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amin_2_f64_x_untied, svfloat64_t,
		z0 = svamin_n_f64_x (p0, z1, 2),
		z0 = svamin_x (p0, z1, 2))

/*
** ptrue_amin_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_f64_x_tied1, svfloat64_t,
		z0 = svamin_f64_x (svptrue_b64 (), z0, z1),
		z0 = svamin_x (svptrue_b64 (), z0, z1))

/*
** ptrue_amin_f64_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_f64_x_tied2, svfloat64_t,
		z0 = svamin_f64_x (svptrue_b64 (), z1, z0),
		z0 = svamin_x (svptrue_b64 (), z1, z0))

/*
** ptrue_amin_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_f64_x_untied, svfloat64_t,
		z0 = svamin_f64_x (svptrue_b64 (), z1, z2),
		z0 = svamin_x (svptrue_b64 (), z1, z2))

/*
** ptrue_amin_0_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_0_f64_x_tied1, svfloat64_t,
		z0 = svamin_n_f64_x (svptrue_b64 (), z0, 0),
		z0 = svamin_x (svptrue_b64 (), z0, 0))

/*
** ptrue_amin_0_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_0_f64_x_untied, svfloat64_t,
		z0 = svamin_n_f64_x (svptrue_b64 (), z1, 0),
		z0 = svamin_x (svptrue_b64 (), z1, 0))

/*
** ptrue_amin_1_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_1_f64_x_tied1, svfloat64_t,
		z0 = svamin_n_f64_x (svptrue_b64 (), z0, 1),
		z0 = svamin_x (svptrue_b64 (), z0, 1))

/*
** ptrue_amin_1_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_1_f64_x_untied, svfloat64_t,
		z0 = svamin_n_f64_x (svptrue_b64 (), z1, 1),
		z0 = svamin_x (svptrue_b64 (), z1, 1))

/*
** ptrue_amin_2_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_2_f64_x_tied1, svfloat64_t,
		z0 = svamin_n_f64_x (svptrue_b64 (), z0, 2),
		z0 = svamin_x (svptrue_b64 (), z0, 2))

/*
** ptrue_amin_2_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amin_2_f64_x_untied, svfloat64_t,
		z0 = svamin_n_f64_x (svptrue_b64 (), z1, 2),
		z0 = svamin_x (svptrue_b64 (), z1, 2))
