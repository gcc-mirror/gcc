/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2+faminmax"
#if STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** amax_f16_m_tied1:
**	famax	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (amax_f16_m_tied1, svfloat16_t,
		z0 = svamax_f16_m (p0, z0, z1),
		z0 = svamax_m (p0, z0, z1))

/*
** amax_f16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	...
**	famax	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (amax_f16_m_tied2, svfloat16_t,
		z0 = svamax_f16_m (p0, z1, z0),
		z0 = svamax_m (p0, z1, z0))

/*
** amax_f16_m_untied:
**	...
**	famax	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (amax_f16_m_untied, svfloat16_t,
		z0 = svamax_f16_m (p0, z1, z2),
		z0 = svamax_m (p0, z1, z2))

/*
** amax_h4_f16_m_tied1:
**	mov	(z[0-9]+\.h), h4
**	famax	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (amax_h4_f16_m_tied1, svfloat16_t, __fp16,
		 z0 = svamax_n_f16_m (p0, z0, d4),
		 z0 = svamax_m (p0, z0, d4))

/*
** amax_h4_f16_m_untied:
**	mov	(z[0-9]+\.h), h4
**	...
**	famax	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (amax_h4_f16_m_untied, svfloat16_t, __fp16,
		 z0 = svamax_n_f16_m (p0, z1, d4),
		 z0 = svamax_m (p0, z1, d4))

/*
** amax_0_f16_m_tied1:
**	...
**	famax	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amax_0_f16_m_tied1, svfloat16_t,
		z0 = svamax_n_f16_m (p0, z0, 0),
		z0 = svamax_m (p0, z0, 0))

/*
** amax_0_f16_m_untied:
**	...
**	famax	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amax_0_f16_m_untied, svfloat16_t,
		z0 = svamax_n_f16_m (p0, z1, 0),
		z0 = svamax_m (p0, z1, 0))

/*
** amax_1_f16_m_tied1:
**	...
**	famax	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amax_1_f16_m_tied1, svfloat16_t,
		z0 = svamax_n_f16_m (p0, z0, 1),
		z0 = svamax_m (p0, z0, 1))

/*
** amax_1_f16_m_untied:
**	...
**	famax	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amax_1_f16_m_untied, svfloat16_t,
		z0 = svamax_n_f16_m (p0, z1, 1),
		z0 = svamax_m (p0, z1, 1))

/*
** amax_2_f16_m:
**	...
**	famax	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amax_2_f16_m, svfloat16_t,
		z0 = svamax_n_f16_m (p0, z0, 2),
		z0 = svamax_m (p0, z0, 2))

/*
** amax_f16_z_tied1:
**	...
**	famax	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (amax_f16_z_tied1, svfloat16_t,
		z0 = svamax_f16_z (p0, z0, z1),
		z0 = svamax_z (p0, z0, z1))

/*
** amax_f16_z_tied2:
**	...
**	famax	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (amax_f16_z_tied2, svfloat16_t,
		z0 = svamax_f16_z (p0, z1, z0),
		z0 = svamax_z (p0, z1, z0))

/*
** amax_f16_z_untied:
** (
**	...
**	famax	z0\.h, p0/m, z0\.h, z2\.h
** |
**	...
**	famax	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (amax_f16_z_untied, svfloat16_t,
		z0 = svamax_f16_z (p0, z1, z2),
		z0 = svamax_z (p0, z1, z2))

/*
** amax_h4_f16_z_tied1:
**	mov	(z[0-9]+\.h), h4
**	...
**	famax	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (amax_h4_f16_z_tied1, svfloat16_t, __fp16,
		 z0 = svamax_n_f16_z (p0, z0, d4),
		 z0 = svamax_z (p0, z0, d4))

/*
** amax_h4_f16_z_untied:
**	mov	(z[0-9]+\.h), h4
** (
**	...
**	famax	z0\.h, p0/m, z0\.h, \1
** |
**	...
**	famax	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZD (amax_h4_f16_z_untied, svfloat16_t, __fp16,
		 z0 = svamax_n_f16_z (p0, z1, d4),
		 z0 = svamax_z (p0, z1, d4))

/*
** amax_0_f16_z_tied1:
**	...
**	famax	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amax_0_f16_z_tied1, svfloat16_t,
		z0 = svamax_n_f16_z (p0, z0, 0),
		z0 = svamax_z (p0, z0, 0))

/*
** amax_0_f16_z_untied:
**	...
**	famax	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amax_0_f16_z_untied, svfloat16_t,
		z0 = svamax_n_f16_z (p0, z1, 0),
		z0 = svamax_z (p0, z1, 0))

/*
** amax_1_f16_z_tied1:
**	...
**	famax	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amax_1_f16_z_tied1, svfloat16_t,
		z0 = svamax_n_f16_z (p0, z0, 1),
		z0 = svamax_z (p0, z0, 1))

/*
** amax_1_f16_z_untied:
**	...
**	famax	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amax_1_f16_z_untied, svfloat16_t,
		z0 = svamax_n_f16_z (p0, z1, 1),
		z0 = svamax_z (p0, z1, 1))

/*
** amax_2_f16_z:
**	...
**	famax	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amax_2_f16_z, svfloat16_t,
		z0 = svamax_n_f16_z (p0, z0, 2),
		z0 = svamax_z (p0, z0, 2))

/*
** amax_f16_x_tied1:
**	famax	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (amax_f16_x_tied1, svfloat16_t,
		z0 = svamax_f16_x (p0, z0, z1),
		z0 = svamax_x (p0, z0, z1))

/*
** amax_f16_x_tied2:
**	famax	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (amax_f16_x_tied2, svfloat16_t,
		z0 = svamax_f16_x (p0, z1, z0),
		z0 = svamax_x (p0, z1, z0))

/*
** amax_f16_x_untied:
** (
**	...
**	famax	z0\.h, p0/m, z0\.h, z2\.h
** |
**	...
**	famax	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (amax_f16_x_untied, svfloat16_t,
		z0 = svamax_f16_x (p0, z1, z2),
		z0 = svamax_x (p0, z1, z2))

/*
** amax_h4_f16_x_tied1:
**	mov	(z[0-9]+\.h), h4
**	famax	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (amax_h4_f16_x_tied1, svfloat16_t, __fp16,
		 z0 = svamax_n_f16_x (p0, z0, d4),
		 z0 = svamax_x (p0, z0, d4))

/*
** amax_h4_f16_x_untied:
**	mov	z0\.h, h4
**	famax	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZD (amax_h4_f16_x_untied, svfloat16_t, __fp16,
		 z0 = svamax_n_f16_x (p0, z1, d4),
		 z0 = svamax_x (p0, z1, d4))

/*
** amax_0_f16_x_tied1:
**	...
**	famax	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amax_0_f16_x_tied1, svfloat16_t,
		z0 = svamax_n_f16_x (p0, z0, 0),
		z0 = svamax_x (p0, z0, 0))

/*
** amax_0_f16_x_untied:
**	...
**	famax	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amax_0_f16_x_untied, svfloat16_t,
		z0 = svamax_n_f16_x (p0, z1, 0),
		z0 = svamax_x (p0, z1, 0))

/*
** amax_1_f16_x_tied1:
**	...
**	famax	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amax_1_f16_x_tied1, svfloat16_t,
		z0 = svamax_n_f16_x (p0, z0, 1),
		z0 = svamax_x (p0, z0, 1))

/*
** amax_1_f16_x_untied:
**	...
**	famax	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amax_1_f16_x_untied, svfloat16_t,
		z0 = svamax_n_f16_x (p0, z1, 1),
		z0 = svamax_x (p0, z1, 1))

/*
** amax_2_f16_x_tied1:
**	...
**	famax	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amax_2_f16_x_tied1, svfloat16_t,
		z0 = svamax_n_f16_x (p0, z0, 2),
		z0 = svamax_x (p0, z0, 2))

/*
** amax_2_f16_x_untied:
**	...
**	famax	z0\.h, p0/m, z0\.h, z[0-9]+\.h
**	ret
*/
TEST_UNIFORM_Z (amax_2_f16_x_untied, svfloat16_t,
		z0 = svamax_n_f16_x (p0, z1, 2),
		z0 = svamax_x (p0, z1, 2))

/*
** ptrue_amax_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_f16_x_tied1, svfloat16_t,
		z0 = svamax_f16_x (svptrue_b16 (), z0, z1),
		z0 = svamax_x (svptrue_b16 (), z0, z1))

/*
** ptrue_amax_f16_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_f16_x_tied2, svfloat16_t,
		z0 = svamax_f16_x (svptrue_b16 (), z1, z0),
		z0 = svamax_x (svptrue_b16 (), z1, z0))

/*
** ptrue_amax_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_f16_x_untied, svfloat16_t,
		z0 = svamax_f16_x (svptrue_b16 (), z1, z2),
		z0 = svamax_x (svptrue_b16 (), z1, z2))

/*
** ptrue_amax_0_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_0_f16_x_tied1, svfloat16_t,
		z0 = svamax_n_f16_x (svptrue_b16 (), z0, 0),
		z0 = svamax_x (svptrue_b16 (), z0, 0))

/*
** ptrue_amax_0_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_0_f16_x_untied, svfloat16_t,
		z0 = svamax_n_f16_x (svptrue_b16 (), z1, 0),
		z0 = svamax_x (svptrue_b16 (), z1, 0))

/*
** ptrue_amax_1_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_1_f16_x_tied1, svfloat16_t,
		z0 = svamax_n_f16_x (svptrue_b16 (), z0, 1),
		z0 = svamax_x (svptrue_b16 (), z0, 1))

/*
** ptrue_amax_1_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_1_f16_x_untied, svfloat16_t,
		z0 = svamax_n_f16_x (svptrue_b16 (), z1, 1),
		z0 = svamax_x (svptrue_b16 (), z1, 1))

/*
** ptrue_amax_2_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_2_f16_x_tied1, svfloat16_t,
		z0 = svamax_n_f16_x (svptrue_b16 (), z0, 2),
		z0 = svamax_x (svptrue_b16 (), z0, 2))

/*
** ptrue_amax_2_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_amax_2_f16_x_untied, svfloat16_t,
		z0 = svamax_n_f16_x (svptrue_b16 (), z1, 2),
		z0 = svamax_x (svptrue_b16 (), z1, 2))
