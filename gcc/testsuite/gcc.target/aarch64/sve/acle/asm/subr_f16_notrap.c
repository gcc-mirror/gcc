/* { dg-additional-options "-fno-trapping-math" } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** subr_f16_m_tied1:
**	fsubr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (subr_f16_m_tied1, svfloat16_t,
		z0 = svsubr_f16_m (p0, z0, z1),
		z0 = svsubr_m (p0, z0, z1))

/*
** subr_f16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fsubr	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (subr_f16_m_tied2, svfloat16_t,
		z0 = svsubr_f16_m (p0, z1, z0),
		z0 = svsubr_m (p0, z1, z0))

/*
** subr_f16_m_untied:
**	movprfx	z0, z1
**	fsubr	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (subr_f16_m_untied, svfloat16_t,
		z0 = svsubr_f16_m (p0, z1, z2),
		z0 = svsubr_m (p0, z1, z2))

/*
** subr_h4_f16_m_tied1:
**	mov	(z[0-9]+\.h), h4
**	fsubr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (subr_h4_f16_m_tied1, svfloat16_t, __fp16,
		 z0 = svsubr_n_f16_m (p0, z0, d4),
		 z0 = svsubr_m (p0, z0, d4))

/*
** subr_h4_f16_m_untied:
**	mov	(z[0-9]+\.h), h4
**	movprfx	z0, z1
**	fsubr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (subr_h4_f16_m_untied, svfloat16_t, __fp16,
		 z0 = svsubr_n_f16_m (p0, z1, d4),
		 z0 = svsubr_m (p0, z1, d4))

/*
** subr_1_f16_m_tied1:
**	fsubr	z0\.h, p0/m, z0\.h, #1\.0
**	ret
*/
TEST_UNIFORM_Z (subr_1_f16_m_tied1, svfloat16_t,
		z0 = svsubr_n_f16_m (p0, z0, 1),
		z0 = svsubr_m (p0, z0, 1))

/*
** subr_1_f16_m_untied:
**	movprfx	z0, z1
**	fsubr	z0\.h, p0/m, z0\.h, #1\.0
**	ret
*/
TEST_UNIFORM_Z (subr_1_f16_m_untied, svfloat16_t,
		z0 = svsubr_n_f16_m (p0, z1, 1),
		z0 = svsubr_m (p0, z1, 1))

/*
** subr_0p5_f16_m_tied1:
**	fsubr	z0\.h, p0/m, z0\.h, #0\.5
**	ret
*/
TEST_UNIFORM_Z (subr_0p5_f16_m_tied1, svfloat16_t,
		z0 = svsubr_n_f16_m (p0, z0, 0.5),
		z0 = svsubr_m (p0, z0, 0.5))

/*
** subr_0p5_f16_m_untied:
**	movprfx	z0, z1
**	fsubr	z0\.h, p0/m, z0\.h, #0\.5
**	ret
*/
TEST_UNIFORM_Z (subr_0p5_f16_m_untied, svfloat16_t,
		z0 = svsubr_n_f16_m (p0, z1, 0.5),
		z0 = svsubr_m (p0, z1, 0.5))

/*
** subr_m1_f16_m_tied1:
**	fmov	(z[0-9]+\.h), #-1\.0(?:e\+0)?
**	fsubr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (subr_m1_f16_m_tied1, svfloat16_t,
		z0 = svsubr_n_f16_m (p0, z0, -1),
		z0 = svsubr_m (p0, z0, -1))

/*
** subr_m1_f16_m_untied: { xfail *-*-* }
**	fmov	(z[0-9]+\.h), #-1\.0(?:e\+0)?
**	movprfx	z0, z1
**	fsubr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (subr_m1_f16_m_untied, svfloat16_t,
		z0 = svsubr_n_f16_m (p0, z1, -1),
		z0 = svsubr_m (p0, z1, -1))

/*
** subr_f16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	fsubr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (subr_f16_z_tied1, svfloat16_t,
		z0 = svsubr_f16_z (p0, z0, z1),
		z0 = svsubr_z (p0, z0, z1))

/*
** subr_f16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	fsub	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (subr_f16_z_tied2, svfloat16_t,
		z0 = svsubr_f16_z (p0, z1, z0),
		z0 = svsubr_z (p0, z1, z0))

/*
** subr_f16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	fsubr	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0\.h, p0/z, z2\.h
**	fsub	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (subr_f16_z_untied, svfloat16_t,
		z0 = svsubr_f16_z (p0, z1, z2),
		z0 = svsubr_z (p0, z1, z2))

/*
** subr_h4_f16_z_tied1:
**	mov	(z[0-9]+\.h), h4
**	movprfx	z0\.h, p0/z, z0\.h
**	fsubr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (subr_h4_f16_z_tied1, svfloat16_t, __fp16,
		 z0 = svsubr_n_f16_z (p0, z0, d4),
		 z0 = svsubr_z (p0, z0, d4))

/*
** subr_h4_f16_z_untied:
**	mov	(z[0-9]+\.h), h4
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	fsubr	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	fsub	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZD (subr_h4_f16_z_untied, svfloat16_t, __fp16,
		 z0 = svsubr_n_f16_z (p0, z1, d4),
		 z0 = svsubr_z (p0, z1, d4))

/*
** subr_1_f16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	fsubr	z0\.h, p0/m, z0\.h, #1\.0
**	ret
*/
TEST_UNIFORM_Z (subr_1_f16_z_tied1, svfloat16_t,
		z0 = svsubr_n_f16_z (p0, z0, 1),
		z0 = svsubr_z (p0, z0, 1))

/*
** subr_1_f16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	fsubr	z0\.h, p0/m, z0\.h, #1\.0
**	ret
*/
TEST_UNIFORM_Z (subr_1_f16_z_untied, svfloat16_t,
		z0 = svsubr_n_f16_z (p0, z1, 1),
		z0 = svsubr_z (p0, z1, 1))

/*
** subr_0p5_f16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	fsubr	z0\.h, p0/m, z0\.h, #0\.5
**	ret
*/
TEST_UNIFORM_Z (subr_0p5_f16_z_tied1, svfloat16_t,
		z0 = svsubr_n_f16_z (p0, z0, 0.5),
		z0 = svsubr_z (p0, z0, 0.5))

/*
** subr_0p5_f16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	fsubr	z0\.h, p0/m, z0\.h, #0\.5
**	ret
*/
TEST_UNIFORM_Z (subr_0p5_f16_z_untied, svfloat16_t,
		z0 = svsubr_n_f16_z (p0, z1, 0.5),
		z0 = svsubr_z (p0, z1, 0.5))

/*
** subr_m1_f16_z_tied1:
**	fmov	(z[0-9]+\.h), #-1\.0(?:e\+0)?
**	movprfx	z0\.h, p0/z, z0\.h
**	fsubr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (subr_m1_f16_z_tied1, svfloat16_t,
		z0 = svsubr_n_f16_z (p0, z0, -1),
		z0 = svsubr_z (p0, z0, -1))

/*
** subr_m1_f16_z_untied:
**	fmov	(z[0-9]+\.h), #-1\.0(?:e\+0)?
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	fsubr	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	fsub	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (subr_m1_f16_z_untied, svfloat16_t,
		z0 = svsubr_n_f16_z (p0, z1, -1),
		z0 = svsubr_z (p0, z1, -1))

/*
** subr_f16_x_tied1:
**	fsub	z0\.h, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (subr_f16_x_tied1, svfloat16_t,
		z0 = svsubr_f16_x (p0, z0, z1),
		z0 = svsubr_x (p0, z0, z1))

/*
** subr_f16_x_tied2:
**	fsub	z0\.h, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (subr_f16_x_tied2, svfloat16_t,
		z0 = svsubr_f16_x (p0, z1, z0),
		z0 = svsubr_x (p0, z1, z0))

/*
** subr_f16_x_untied:
**	fsub	z0\.h, z2\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (subr_f16_x_untied, svfloat16_t,
		z0 = svsubr_f16_x (p0, z1, z2),
		z0 = svsubr_x (p0, z1, z2))

/*
** subr_h4_f16_x_tied1:
**	mov	(z[0-9]+\.h), h4
**	fsub	z0\.h, \1, z0\.h
**	ret
*/
TEST_UNIFORM_ZD (subr_h4_f16_x_tied1, svfloat16_t, __fp16,
		 z0 = svsubr_n_f16_x (p0, z0, d4),
		 z0 = svsubr_x (p0, z0, d4))

/*
** subr_h4_f16_x_untied:
**	mov	(z[0-9]+\.h), h4
**	fsub	z0\.h, \1, z1\.h
**	ret
*/
TEST_UNIFORM_ZD (subr_h4_f16_x_untied, svfloat16_t, __fp16,
		 z0 = svsubr_n_f16_x (p0, z1, d4),
		 z0 = svsubr_x (p0, z1, d4))

/*
** subr_1_f16_x_tied1:
**	fsubr	z0\.h, p0/m, z0\.h, #1\.0
**	ret
*/
TEST_UNIFORM_Z (subr_1_f16_x_tied1, svfloat16_t,
		z0 = svsubr_n_f16_x (p0, z0, 1),
		z0 = svsubr_x (p0, z0, 1))

/*
** subr_1_f16_x_untied:
**	movprfx	z0, z1
**	fsubr	z0\.h, p0/m, z0\.h, #1\.0
**	ret
*/
TEST_UNIFORM_Z (subr_1_f16_x_untied, svfloat16_t,
		z0 = svsubr_n_f16_x (p0, z1, 1),
		z0 = svsubr_x (p0, z1, 1))

/*
** subr_0p5_f16_x_tied1:
**	fsubr	z0\.h, p0/m, z0\.h, #0\.5
**	ret
*/
TEST_UNIFORM_Z (subr_0p5_f16_x_tied1, svfloat16_t,
		z0 = svsubr_n_f16_x (p0, z0, 0.5),
		z0 = svsubr_x (p0, z0, 0.5))

/*
** subr_0p5_f16_x_untied:
**	movprfx	z0, z1
**	fsubr	z0\.h, p0/m, z0\.h, #0\.5
**	ret
*/
TEST_UNIFORM_Z (subr_0p5_f16_x_untied, svfloat16_t,
		z0 = svsubr_n_f16_x (p0, z1, 0.5),
		z0 = svsubr_x (p0, z1, 0.5))

/*
** subr_m1_f16_x_tied1:
**	fmov	(z[0-9]+\.h), #-1\.0(?:e\+0)?
**	fsub	z0\.h, \1, z0\.h
**	ret
*/
TEST_UNIFORM_Z (subr_m1_f16_x_tied1, svfloat16_t,
		z0 = svsubr_n_f16_x (p0, z0, -1),
		z0 = svsubr_x (p0, z0, -1))

/*
** subr_m1_f16_x_untied:
**	fmov	(z[0-9]+\.h), #-1\.0(?:e\+0)?
**	fsub	z0\.h, \1, z1\.h
**	ret
*/
TEST_UNIFORM_Z (subr_m1_f16_x_untied, svfloat16_t,
		z0 = svsubr_n_f16_x (p0, z1, -1),
		z0 = svsubr_x (p0, z1, -1))

/*
** ptrue_subr_f16_x_tied1:
**	fsub	z0\.h, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (ptrue_subr_f16_x_tied1, svfloat16_t,
		z0 = svsubr_f16_x (svptrue_b16 (), z0, z1),
		z0 = svsubr_x (svptrue_b16 (), z0, z1))

/*
** ptrue_subr_f16_x_tied2:
**	fsub	z0\.h, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (ptrue_subr_f16_x_tied2, svfloat16_t,
		z0 = svsubr_f16_x (svptrue_b16 (), z1, z0),
		z0 = svsubr_x (svptrue_b16 (), z1, z0))

/*
** ptrue_subr_f16_x_untied:
**	fsub	z0\.h, z2\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (ptrue_subr_f16_x_untied, svfloat16_t,
		z0 = svsubr_f16_x (svptrue_b16 (), z1, z2),
		z0 = svsubr_x (svptrue_b16 (), z1, z2))

/*
** ptrue_subr_1_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_subr_1_f16_x_tied1, svfloat16_t,
		z0 = svsubr_n_f16_x (svptrue_b16 (), z0, 1),
		z0 = svsubr_x (svptrue_b16 (), z0, 1))

/*
** ptrue_subr_1_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_subr_1_f16_x_untied, svfloat16_t,
		z0 = svsubr_n_f16_x (svptrue_b16 (), z1, 1),
		z0 = svsubr_x (svptrue_b16 (), z1, 1))

/*
** ptrue_subr_0p5_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_subr_0p5_f16_x_tied1, svfloat16_t,
		z0 = svsubr_n_f16_x (svptrue_b16 (), z0, 0.5),
		z0 = svsubr_x (svptrue_b16 (), z0, 0.5))

/*
** ptrue_subr_0p5_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_subr_0p5_f16_x_untied, svfloat16_t,
		z0 = svsubr_n_f16_x (svptrue_b16 (), z1, 0.5),
		z0 = svsubr_x (svptrue_b16 (), z1, 0.5))

/*
** ptrue_subr_m1_f16_x_tied1:
**	fmov	(z[0-9]+\.h), #-1\.0(?:e\+0)?
**	fsub	z0\.h, \1, z0\.h
**	ret
*/
TEST_UNIFORM_Z (ptrue_subr_m1_f16_x_tied1, svfloat16_t,
		z0 = svsubr_n_f16_x (svptrue_b16 (), z0, -1),
		z0 = svsubr_x (svptrue_b16 (), z0, -1))

/*
** ptrue_subr_m1_f16_x_untied:
**	fmov	(z[0-9]+\.h), #-1\.0(?:e\+0)?
**	fsub	z0\.h, \1, z1\.h
**	ret
*/
TEST_UNIFORM_Z (ptrue_subr_m1_f16_x_untied, svfloat16_t,
		z0 = svsubr_n_f16_x (svptrue_b16 (), z1, -1),
		z0 = svsubr_x (svptrue_b16 (), z1, -1))
