/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve+faminmax"

/*
** amax_f64_m_tied1:
**	famax	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (amax_f64_m_tied1, svfloat64_t,
		z0 = svamax_f64_m (p0, z0, z1),
		z0 = svamax_m (p0, z0, z1))

/*
** amax_f64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	...
**	famax	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (amax_f64_m_tied2, svfloat64_t,
		z0 = svamax_f64_m (p0, z1, z0),
		z0 = svamax_m (p0, z1, z0))

/*
** amax_f64_m_untied:
**	...
**	famax	z0\.d, p0/m, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (amax_f64_m_untied, svfloat64_t,
		z0 = svamax_f64_m (p0, z1, z2),
		z0 = svamax_m (p0, z1, z2))

/*
** amax_d4_f64_m_tied1:
**	mov	(z[0-9]+\.d), d4
**	famax	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (amax_d4_f64_m_tied1, svfloat64_t, double,
		 z0 = svamax_n_f64_m (p0, z0, d4),
		 z0 = svamax_m (p0, z0, d4))

/*
** amax_d4_f64_m_untied:
**	mov	(z[0-9]+\.d), d4
**	...
**	famax	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (amax_d4_f64_m_untied, svfloat64_t, double,
		 z0 = svamax_n_f64_m (p0, z1, d4),
		 z0 = svamax_m (p0, z1, d4))

/*
** amax_0_f64_m_tied1:
**	...
**	famax	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amax_0_f64_m_tied1, svfloat64_t,
		z0 = svamax_n_f64_m (p0, z0, 0),
		z0 = svamax_m (p0, z0, 0))

/*
** amax_0_f64_m_untied:
**	...
**	famax	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amax_0_f64_m_untied, svfloat64_t,
		z0 = svamax_n_f64_m (p0, z1, 0),
		z0 = svamax_m (p0, z1, 0))

/*
** amax_1_f64_m_tied1:
**	...
**	famax	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amax_1_f64_m_tied1, svfloat64_t,
		z0 = svamax_n_f64_m (p0, z0, 1),
		z0 = svamax_m (p0, z0, 1))

/*
** amax_1_f64_m_untied:
**	...
**	famax	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amax_1_f64_m_untied, svfloat64_t,
		z0 = svamax_n_f64_m (p0, z1, 1),
		z0 = svamax_m (p0, z1, 1))

/*
** amax_2_f64_m:
**	...
**	famax	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amax_2_f64_m, svfloat64_t,
		z0 = svamax_n_f64_m (p0, z0, 2),
		z0 = svamax_m (p0, z0, 2))

/*
** amax_f64_z_tied1:
**	...
**	famax	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (amax_f64_z_tied1, svfloat64_t,
		z0 = svamax_f64_z (p0, z0, z1),
		z0 = svamax_z (p0, z0, z1))

/*
** amax_f64_z_tied2:
**	...
**	famax	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (amax_f64_z_tied2, svfloat64_t,
		z0 = svamax_f64_z (p0, z1, z0),
		z0 = svamax_z (p0, z1, z0))

/*
** amax_f64_z_untied:
** (
**	...
**	famax	z0\.d, p0/m, z0\.d, z2\.d
** |
**	...
**	famax	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (amax_f64_z_untied, svfloat64_t,
		z0 = svamax_f64_z (p0, z1, z2),
		z0 = svamax_z (p0, z1, z2))

/*
** amax_d4_f64_z_tied1:
**	mov	(z[0-9]+\.d), d4
**	...
**	famax	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (amax_d4_f64_z_tied1, svfloat64_t, double,
		 z0 = svamax_n_f64_z (p0, z0, d4),
		 z0 = svamax_z (p0, z0, d4))

/*
** amax_d4_f64_z_untied:
**	mov	(z[0-9]+\.d), d4
** (
**	...
**	famax	z0\.d, p0/m, z0\.d, \1
** |
**	...
**	famax	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZD (amax_d4_f64_z_untied, svfloat64_t, double,
		 z0 = svamax_n_f64_z (p0, z1, d4),
		 z0 = svamax_z (p0, z1, d4))

/*
** amax_0_f64_z_tied1:
**	...
**	famax	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amax_0_f64_z_tied1, svfloat64_t,
		z0 = svamax_n_f64_z (p0, z0, 0),
		z0 = svamax_z (p0, z0, 0))

/*
** amax_0_f64_z_untied:
**	...
**	famax	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amax_0_f64_z_untied, svfloat64_t,
		z0 = svamax_n_f64_z (p0, z1, 0),
		z0 = svamax_z (p0, z1, 0))

/*
** amax_1_f64_z_tied1:
**	...
**	famax	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amax_1_f64_z_tied1, svfloat64_t,
		z0 = svamax_n_f64_z (p0, z0, 1),
		z0 = svamax_z (p0, z0, 1))

/*
** amax_1_f64_z_untied:
**	...
**	famax	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amax_1_f64_z_untied, svfloat64_t,
		z0 = svamax_n_f64_z (p0, z1, 1),
		z0 = svamax_z (p0, z1, 1))

/*
** amax_2_f64_z:
**	...
**	famax	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amax_2_f64_z, svfloat64_t,
		z0 = svamax_n_f64_z (p0, z0, 2),
		z0 = svamax_z (p0, z0, 2))

/*
** amax_f64_x_tied1:
**	famax	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (amax_f64_x_tied1, svfloat64_t,
		z0 = svamax_f64_x (p0, z0, z1),
		z0 = svamax_x (p0, z0, z1))

/*
** amax_f64_x_tied2:
**	famax	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (amax_f64_x_tied2, svfloat64_t,
		z0 = svamax_f64_x (p0, z1, z0),
		z0 = svamax_x (p0, z1, z0))

/*
** amax_f64_x_untied:
** (
**	...
**	famax	z0\.d, p0/m, z0\.d, z2\.d
** |
**	...
**	famax	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (amax_f64_x_untied, svfloat64_t,
		z0 = svamax_f64_x (p0, z1, z2),
		z0 = svamax_x (p0, z1, z2))

/*
** amax_d4_f64_x_tied1:
**	mov	(z[0-9]+\.d), d4
**	famax	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZD (amax_d4_f64_x_tied1, svfloat64_t, double,
		 z0 = svamax_n_f64_x (p0, z0, d4),
		 z0 = svamax_x (p0, z0, d4))

/*
** amax_d4_f64_x_untied:
**	mov	z0\.d, d4
**	famax	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_ZD (amax_d4_f64_x_untied, svfloat64_t, double,
		 z0 = svamax_n_f64_x (p0, z1, d4),
		 z0 = svamax_x (p0, z1, d4))

/*
** amax_0_f64_x_tied1:
**	...
**	famax	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amax_0_f64_x_tied1, svfloat64_t,
		z0 = svamax_n_f64_x (p0, z0, 0),
		z0 = svamax_x (p0, z0, 0))

/*
** amax_0_f64_x_untied:
**	...
**	famax	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amax_0_f64_x_untied, svfloat64_t,
		z0 = svamax_n_f64_x (p0, z1, 0),
		z0 = svamax_x (p0, z1, 0))

/*
** amax_1_f64_x_tied1:
**	...
**	famax	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amax_1_f64_x_tied1, svfloat64_t,
		z0 = svamax_n_f64_x (p0, z0, 1),
		z0 = svamax_x (p0, z0, 1))

/*
** amax_1_f64_x_untied:
**	...
**	famax	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amax_1_f64_x_untied, svfloat64_t,
		z0 = svamax_n_f64_x (p0, z1, 1),
		z0 = svamax_x (p0, z1, 1))

/*
** amax_2_f64_x_tied1:
**	...
**	famax	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amax_2_f64_x_tied1, svfloat64_t,
		z0 = svamax_n_f64_x (p0, z0, 2),
		z0 = svamax_x (p0, z0, 2))

/*
** amax_2_f64_x_untied:
**	...
**	famax	z0\.d, p0/m, z0\.d, z[0-9]+\.d
**	ret
*/
TEST_UNIFORM_Z (amax_2_f64_x_untied, svfloat64_t,
		z0 = svamax_n_f64_x (p0, z1, 2),
		z0 = svamax_x (p0, z1, 2))

/*
** ptrue_amax_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_f64_x_tied1, svfloat64_t,
		z0 = svamax_f64_x (svptrue_b64 (), z0, z1),
		z0 = svamax_x (svptrue_b64 (), z0, z1))

/*
** ptrue_amax_f64_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_f64_x_tied2, svfloat64_t,
		z0 = svamax_f64_x (svptrue_b64 (), z1, z0),
		z0 = svamax_x (svptrue_b64 (), z1, z0))

/*
** ptrue_amax_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_f64_x_untied, svfloat64_t,
		z0 = svamax_f64_x (svptrue_b64 (), z1, z2),
		z0 = svamax_x (svptrue_b64 (), z1, z2))

/*
** ptrue_amax_0_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_0_f64_x_tied1, svfloat64_t,
		z0 = svamax_n_f64_x (svptrue_b64 (), z0, 0),
		z0 = svamax_x (svptrue_b64 (), z0, 0))

/*
** ptrue_amax_0_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_0_f64_x_untied, svfloat64_t,
		z0 = svamax_n_f64_x (svptrue_b64 (), z1, 0),
		z0 = svamax_x (svptrue_b64 (), z1, 0))

/*
** ptrue_amax_1_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_1_f64_x_tied1, svfloat64_t,
		z0 = svamax_n_f64_x (svptrue_b64 (), z0, 1),
		z0 = svamax_x (svptrue_b64 (), z0, 1))

/*
** ptrue_amax_1_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_1_f64_x_untied, svfloat64_t,
		z0 = svamax_n_f64_x (svptrue_b64 (), z1, 1),
		z0 = svamax_x (svptrue_b64 (), z1, 1))

/*
** ptrue_amax_2_f64_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_2_f64_x_tied1, svfloat64_t,
		z0 = svamax_n_f64_x (svptrue_b64 (), z0, 2),
		z0 = svamax_x (svptrue_b64 (), z0, 2))

/*
** ptrue_amax_2_f64_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_2_f64_x_untied, svfloat64_t,
		z0 = svamax_n_f64_x (svptrue_b64 (), z1, 2),
		z0 = svamax_x (svptrue_b64 (), z1, 2))
