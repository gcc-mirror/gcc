/* { dg-additional-options "-fno-trapping-math" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** add_f16_m_tied1:
**	fadd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (add_f16_m_tied1, svfloat16_t,
		z0 = svadd_f16_m (p0, z0, z1),
		z0 = svadd_m (p0, z0, z1))

/*
** add_f16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fadd	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (add_f16_m_tied2, svfloat16_t,
		z0 = svadd_f16_m (p0, z1, z0),
		z0 = svadd_m (p0, z1, z0))

/*
** add_f16_m_untied:
**	movprfx	z0, z1
**	fadd	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (add_f16_m_untied, svfloat16_t,
		z0 = svadd_f16_m (p0, z1, z2),
		z0 = svadd_m (p0, z1, z2))

/*
** add_h4_f16_m_tied1:
**	mov	(z[0-9]+\.h), h4
**	fadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (add_h4_f16_m_tied1, svfloat16_t, __fp16,
		 z0 = svadd_n_f16_m (p0, z0, d4),
		 z0 = svadd_m (p0, z0, d4))

/*
** add_h4_f16_m_untied:
**	mov	(z[0-9]+\.h), h4
**	movprfx	z0, z1
**	fadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (add_h4_f16_m_untied, svfloat16_t, __fp16,
		 z0 = svadd_n_f16_m (p0, z1, d4),
		 z0 = svadd_m (p0, z1, d4))

/*
** add_1_f16_m_tied1:
**	fadd	z0\.h, p0/m, z0\.h, #1\.0
**	ret
*/
TEST_UNIFORM_Z (add_1_f16_m_tied1, svfloat16_t,
		z0 = svadd_n_f16_m (p0, z0, 1),
		z0 = svadd_m (p0, z0, 1))

/*
** add_1_f16_m_untied:
**	movprfx	z0, z1
**	fadd	z0\.h, p0/m, z0\.h, #1\.0
**	ret
*/
TEST_UNIFORM_Z (add_1_f16_m_untied, svfloat16_t,
		z0 = svadd_n_f16_m (p0, z1, 1),
		z0 = svadd_m (p0, z1, 1))

/*
** add_0p5_f16_m_tied1:
**	fadd	z0\.h, p0/m, z0\.h, #0\.5
**	ret
*/
TEST_UNIFORM_Z (add_0p5_f16_m_tied1, svfloat16_t,
		z0 = svadd_n_f16_m (p0, z0, 0.5),
		z0 = svadd_m (p0, z0, 0.5))

/*
** add_0p5_f16_m_untied:
**	movprfx	z0, z1
**	fadd	z0\.h, p0/m, z0\.h, #0\.5
**	ret
*/
TEST_UNIFORM_Z (add_0p5_f16_m_untied, svfloat16_t,
		z0 = svadd_n_f16_m (p0, z1, 0.5),
		z0 = svadd_m (p0, z1, 0.5))

/*
** add_m1_f16_m_tied1:
**	fsub	z0\.h, p0/m, z0\.h, #1\.0
**	ret
*/
TEST_UNIFORM_Z (add_m1_f16_m_tied1, svfloat16_t,
		z0 = svadd_n_f16_m (p0, z0, -1),
		z0 = svadd_m (p0, z0, -1))

/*
** add_m1_f16_m_untied:
**	movprfx	z0, z1
**	fsub	z0\.h, p0/m, z0\.h, #1\.0
**	ret
*/
TEST_UNIFORM_Z (add_m1_f16_m_untied, svfloat16_t,
		z0 = svadd_n_f16_m (p0, z1, -1),
		z0 = svadd_m (p0, z1, -1))

/*
** add_m0p5_f16_m_tied1:
**	fsub	z0\.h, p0/m, z0\.h, #0\.5
**	ret
*/
TEST_UNIFORM_Z (add_m0p5_f16_m_tied1, svfloat16_t,
		z0 = svadd_n_f16_m (p0, z0, -0.5),
		z0 = svadd_m (p0, z0, -0.5))

/*
** add_m0p5_f16_m_untied:
**	movprfx	z0, z1
**	fsub	z0\.h, p0/m, z0\.h, #0\.5
**	ret
*/
TEST_UNIFORM_Z (add_m0p5_f16_m_untied, svfloat16_t,
		z0 = svadd_n_f16_m (p0, z1, -0.5),
		z0 = svadd_m (p0, z1, -0.5))

/*
** add_m2_f16_m:
**	fmov	(z[0-9]+\.h), #-2\.0(?:e\+0)?
**	fadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (add_m2_f16_m, svfloat16_t,
		z0 = svadd_n_f16_m (p0, z0, -2),
		z0 = svadd_m (p0, z0, -2))

/*
** add_f16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	fadd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (add_f16_z_tied1, svfloat16_t,
		z0 = svadd_f16_z (p0, z0, z1),
		z0 = svadd_z (p0, z0, z1))

/*
** add_f16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	fadd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (add_f16_z_tied2, svfloat16_t,
		z0 = svadd_f16_z (p0, z1, z0),
		z0 = svadd_z (p0, z1, z0))

/*
** add_f16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	fadd	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0\.h, p0/z, z2\.h
**	fadd	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (add_f16_z_untied, svfloat16_t,
		z0 = svadd_f16_z (p0, z1, z2),
		z0 = svadd_z (p0, z1, z2))

/*
** add_h4_f16_z_tied1:
**	mov	(z[0-9]+\.h), h4
**	movprfx	z0\.h, p0/z, z0\.h
**	fadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (add_h4_f16_z_tied1, svfloat16_t, __fp16,
		 z0 = svadd_n_f16_z (p0, z0, d4),
		 z0 = svadd_z (p0, z0, d4))

/*
** add_h4_f16_z_untied:
**	mov	(z[0-9]+\.h), h4
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	fadd	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	fadd	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZD (add_h4_f16_z_untied, svfloat16_t, __fp16,
		 z0 = svadd_n_f16_z (p0, z1, d4),
		 z0 = svadd_z (p0, z1, d4))

/*
** add_1_f16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	fadd	z0\.h, p0/m, z0\.h, #1\.0
**	ret
*/
TEST_UNIFORM_Z (add_1_f16_z_tied1, svfloat16_t,
		z0 = svadd_n_f16_z (p0, z0, 1),
		z0 = svadd_z (p0, z0, 1))

/*
** add_1_f16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	fadd	z0\.h, p0/m, z0\.h, #1\.0
**	ret
*/
TEST_UNIFORM_Z (add_1_f16_z_untied, svfloat16_t,
		z0 = svadd_n_f16_z (p0, z1, 1),
		z0 = svadd_z (p0, z1, 1))

/*
** add_0p5_f16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	fadd	z0\.h, p0/m, z0\.h, #0\.5
**	ret
*/
TEST_UNIFORM_Z (add_0p5_f16_z_tied1, svfloat16_t,
		z0 = svadd_n_f16_z (p0, z0, 0.5),
		z0 = svadd_z (p0, z0, 0.5))

/*
** add_0p5_f16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	fadd	z0\.h, p0/m, z0\.h, #0\.5
**	ret
*/
TEST_UNIFORM_Z (add_0p5_f16_z_untied, svfloat16_t,
		z0 = svadd_n_f16_z (p0, z1, 0.5),
		z0 = svadd_z (p0, z1, 0.5))

/*
** add_m1_f16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	fsub	z0\.h, p0/m, z0\.h, #1\.0
**	ret
*/
TEST_UNIFORM_Z (add_m1_f16_z_tied1, svfloat16_t,
		z0 = svadd_n_f16_z (p0, z0, -1),
		z0 = svadd_z (p0, z0, -1))

/*
** add_m1_f16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	fsub	z0\.h, p0/m, z0\.h, #1\.0
**	ret
*/
TEST_UNIFORM_Z (add_m1_f16_z_untied, svfloat16_t,
		z0 = svadd_n_f16_z (p0, z1, -1),
		z0 = svadd_z (p0, z1, -1))

/*
** add_m0p5_f16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	fsub	z0\.h, p0/m, z0\.h, #0\.5
**	ret
*/
TEST_UNIFORM_Z (add_m0p5_f16_z_tied1, svfloat16_t,
		z0 = svadd_n_f16_z (p0, z0, -0.5),
		z0 = svadd_z (p0, z0, -0.5))

/*
** add_m0p5_f16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	fsub	z0\.h, p0/m, z0\.h, #0\.5
**	ret
*/
TEST_UNIFORM_Z (add_m0p5_f16_z_untied, svfloat16_t,
		z0 = svadd_n_f16_z (p0, z1, -0.5),
		z0 = svadd_z (p0, z1, -0.5))

/*
** add_m2_f16_z:
**	fmov	(z[0-9]+\.h), #-2\.0(?:e\+0)?
**	movprfx	z0\.h, p0/z, z0\.h
**	fadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (add_m2_f16_z, svfloat16_t,
		z0 = svadd_n_f16_z (p0, z0, -2),
		z0 = svadd_z (p0, z0, -2))

/*
** add_f16_x_tied1:
**	fadd	z0\.h, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_UNIFORM_Z (add_f16_x_tied1, svfloat16_t,
		z0 = svadd_f16_x (p0, z0, z1),
		z0 = svadd_x (p0, z0, z1))

/*
** add_f16_x_tied2:
**	fadd	z0\.h, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_UNIFORM_Z (add_f16_x_tied2, svfloat16_t,
		z0 = svadd_f16_x (p0, z1, z0),
		z0 = svadd_x (p0, z1, z0))

/*
** add_f16_x_untied:
**	fadd	z0\.h, (z1\.h, z2\.h|z2\.h, z1\.h)
**	ret
*/
TEST_UNIFORM_Z (add_f16_x_untied, svfloat16_t,
		z0 = svadd_f16_x (p0, z1, z2),
		z0 = svadd_x (p0, z1, z2))

/*
** add_h4_f16_x_tied1:
**	mov	(z[0-9]+\.h), h4
**	fadd	z0\.h, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_UNIFORM_ZD (add_h4_f16_x_tied1, svfloat16_t, __fp16,
		 z0 = svadd_n_f16_x (p0, z0, d4),
		 z0 = svadd_x (p0, z0, d4))

/*
** add_h4_f16_x_untied:
**	mov	(z[0-9]+\.h), h4
**	fadd	z0\.h, (z1\.h, \1|\1, z1\.h)
**	ret
*/
TEST_UNIFORM_ZD (add_h4_f16_x_untied, svfloat16_t, __fp16,
		 z0 = svadd_n_f16_x (p0, z1, d4),
		 z0 = svadd_x (p0, z1, d4))

/*
** add_1_f16_x_tied1:
**	fadd	z0\.h, p0/m, z0\.h, #1\.0
**	ret
*/
TEST_UNIFORM_Z (add_1_f16_x_tied1, svfloat16_t,
		z0 = svadd_n_f16_x (p0, z0, 1),
		z0 = svadd_x (p0, z0, 1))

/*
** add_1_f16_x_untied:
**	movprfx	z0, z1
**	fadd	z0\.h, p0/m, z0\.h, #1\.0
**	ret
*/
TEST_UNIFORM_Z (add_1_f16_x_untied, svfloat16_t,
		z0 = svadd_n_f16_x (p0, z1, 1),
		z0 = svadd_x (p0, z1, 1))

/*
** add_0p5_f16_x_tied1:
**	fadd	z0\.h, p0/m, z0\.h, #0\.5
**	ret
*/
TEST_UNIFORM_Z (add_0p5_f16_x_tied1, svfloat16_t,
		z0 = svadd_n_f16_x (p0, z0, 0.5),
		z0 = svadd_x (p0, z0, 0.5))

/*
** add_0p5_f16_x_untied:
**	movprfx	z0, z1
**	fadd	z0\.h, p0/m, z0\.h, #0\.5
**	ret
*/
TEST_UNIFORM_Z (add_0p5_f16_x_untied, svfloat16_t,
		z0 = svadd_n_f16_x (p0, z1, 0.5),
		z0 = svadd_x (p0, z1, 0.5))

/*
** add_m1_f16_x_tied1:
**	fsub	z0\.h, p0/m, z0\.h, #1\.0
**	ret
*/
TEST_UNIFORM_Z (add_m1_f16_x_tied1, svfloat16_t,
		z0 = svadd_n_f16_x (p0, z0, -1),
		z0 = svadd_x (p0, z0, -1))

/*
** add_m1_f16_x_untied:
**	movprfx	z0, z1
**	fsub	z0\.h, p0/m, z0\.h, #1\.0
**	ret
*/
TEST_UNIFORM_Z (add_m1_f16_x_untied, svfloat16_t,
		z0 = svadd_n_f16_x (p0, z1, -1),
		z0 = svadd_x (p0, z1, -1))

/*
** add_m0p5_f16_x_tied1:
**	fsub	z0\.h, p0/m, z0\.h, #0\.5
**	ret
*/
TEST_UNIFORM_Z (add_m0p5_f16_x_tied1, svfloat16_t,
		z0 = svadd_n_f16_x (p0, z0, -0.5),
		z0 = svadd_x (p0, z0, -0.5))

/*
** add_m0p5_f16_x_untied:
**	movprfx	z0, z1
**	fsub	z0\.h, p0/m, z0\.h, #0\.5
**	ret
*/
TEST_UNIFORM_Z (add_m0p5_f16_x_untied, svfloat16_t,
		z0 = svadd_n_f16_x (p0, z1, -0.5),
		z0 = svadd_x (p0, z1, -0.5))

/*
** add_2_f16_x_tied1:
**	fmov	(z[0-9]+\.h), #2\.0(?:e\+0)?
**	fadd	z0\.h, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_UNIFORM_Z (add_2_f16_x_tied1, svfloat16_t,
		z0 = svadd_n_f16_x (p0, z0, 2),
		z0 = svadd_x (p0, z0, 2))

/*
** add_2_f16_x_untied:
**	fmov	(z[0-9]+\.h), #2\.0(?:e\+0)?
**	fadd	z0\.h, (z1\.h, \1|\1, z1\.h)
**	ret
*/
TEST_UNIFORM_Z (add_2_f16_x_untied, svfloat16_t,
		z0 = svadd_n_f16_x (p0, z1, 2),
		z0 = svadd_x (p0, z1, 2))

/*
** ptrue_add_f16_x_tied1:
**	fadd	z0\.h, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_UNIFORM_Z (ptrue_add_f16_x_tied1, svfloat16_t,
		z0 = svadd_f16_x (svptrue_b16 (), z0, z1),
		z0 = svadd_x (svptrue_b16 (), z0, z1))

/*
** ptrue_add_f16_x_tied2:
**	fadd	z0\.h, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_UNIFORM_Z (ptrue_add_f16_x_tied2, svfloat16_t,
		z0 = svadd_f16_x (svptrue_b16 (), z1, z0),
		z0 = svadd_x (svptrue_b16 (), z1, z0))

/*
** ptrue_add_f16_x_untied:
**	fadd	z0\.h, (z1\.h, z2\.h|z2\.h, z1\.h)
**	ret
*/
TEST_UNIFORM_Z (ptrue_add_f16_x_untied, svfloat16_t,
		z0 = svadd_f16_x (svptrue_b16 (), z1, z2),
		z0 = svadd_x (svptrue_b16 (), z1, z2))

/*
** ptrue_add_1_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_add_1_f16_x_tied1, svfloat16_t,
		z0 = svadd_n_f16_x (svptrue_b16 (), z0, 1),
		z0 = svadd_x (svptrue_b16 (), z0, 1))

/*
** ptrue_add_1_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_add_1_f16_x_untied, svfloat16_t,
		z0 = svadd_n_f16_x (svptrue_b16 (), z1, 1),
		z0 = svadd_x (svptrue_b16 (), z1, 1))

/*
** ptrue_add_0p5_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_add_0p5_f16_x_tied1, svfloat16_t,
		z0 = svadd_n_f16_x (svptrue_b16 (), z0, 0.5),
		z0 = svadd_x (svptrue_b16 (), z0, 0.5))

/*
** ptrue_add_0p5_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_add_0p5_f16_x_untied, svfloat16_t,
		z0 = svadd_n_f16_x (svptrue_b16 (), z1, 0.5),
		z0 = svadd_x (svptrue_b16 (), z1, 0.5))

/*
** ptrue_add_m1_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_add_m1_f16_x_tied1, svfloat16_t,
		z0 = svadd_n_f16_x (svptrue_b16 (), z0, -1),
		z0 = svadd_x (svptrue_b16 (), z0, -1))

/*
** ptrue_add_m1_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_add_m1_f16_x_untied, svfloat16_t,
		z0 = svadd_n_f16_x (svptrue_b16 (), z1, -1),
		z0 = svadd_x (svptrue_b16 (), z1, -1))

/*
** ptrue_add_m0p5_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_add_m0p5_f16_x_tied1, svfloat16_t,
		z0 = svadd_n_f16_x (svptrue_b16 (), z0, -0.5),
		z0 = svadd_x (svptrue_b16 (), z0, -0.5))

/*
** ptrue_add_m0p5_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_add_m0p5_f16_x_untied, svfloat16_t,
		z0 = svadd_n_f16_x (svptrue_b16 (), z1, -0.5),
		z0 = svadd_x (svptrue_b16 (), z1, -0.5))

/*
** ptrue_add_2_f16_x_tied1:
**	fmov	(z[0-9]+\.h), #2\.0(?:e\+0)?
**	fadd	z0\.h, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_UNIFORM_Z (ptrue_add_2_f16_x_tied1, svfloat16_t,
		z0 = svadd_n_f16_x (svptrue_b16 (), z0, 2),
		z0 = svadd_x (svptrue_b16 (), z0, 2))

/*
** ptrue_add_2_f16_x_untied:
**	fmov	(z[0-9]+\.h), #2\.0(?:e\+0)?
**	fadd	z0\.h, (z1\.h, \1|\1, z1\.h)
**	ret
*/
TEST_UNIFORM_Z (ptrue_add_2_f16_x_untied, svfloat16_t,
		z0 = svadd_n_f16_x (svptrue_b16 (), z1, 2),
		z0 = svadd_x (svptrue_b16 (), z1, 2))
