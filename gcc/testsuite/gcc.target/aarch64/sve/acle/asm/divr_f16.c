/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** divr_f16_m_tied1:
**	fdivr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (divr_f16_m_tied1, svfloat16_t,
		z0 = svdivr_f16_m (p0, z0, z1),
		z0 = svdivr_m (p0, z0, z1))

/*
** divr_f16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fdivr	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (divr_f16_m_tied2, svfloat16_t,
		z0 = svdivr_f16_m (p0, z1, z0),
		z0 = svdivr_m (p0, z1, z0))

/*
** divr_f16_m_untied:
**	movprfx	z0, z1
**	fdivr	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (divr_f16_m_untied, svfloat16_t,
		z0 = svdivr_f16_m (p0, z1, z2),
		z0 = svdivr_m (p0, z1, z2))

/*
** divr_h4_f16_m_tied1:
**	mov	(z[0-9]+\.h), h4
**	fdivr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (divr_h4_f16_m_tied1, svfloat16_t, __fp16,
		 z0 = svdivr_n_f16_m (p0, z0, d4),
		 z0 = svdivr_m (p0, z0, d4))

/*
** divr_h4_f16_m_untied:
**	mov	(z[0-9]+\.h), h4
**	movprfx	z0, z1
**	fdivr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (divr_h4_f16_m_untied, svfloat16_t, __fp16,
		 z0 = svdivr_n_f16_m (p0, z1, d4),
		 z0 = svdivr_m (p0, z1, d4))

/*
** divr_1_f16_m_tied1:
**	fmov	(z[0-9]+\.h), #1\.0(?:e\+0)?
**	fdivr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (divr_1_f16_m_tied1, svfloat16_t,
		z0 = svdivr_n_f16_m (p0, z0, 1),
		z0 = svdivr_m (p0, z0, 1))

/*
** divr_1_f16_m_untied: { xfail *-*-* }
**	fmov	(z[0-9]+\.h), #1\.0(?:e\+0)?
**	movprfx	z0, z1
**	fdivr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (divr_1_f16_m_untied, svfloat16_t,
		z0 = svdivr_n_f16_m (p0, z1, 1),
		z0 = svdivr_m (p0, z1, 1))

/*
** divr_0p5_f16_m_tied1:
**	fmov	(z[0-9]+\.h), #(?:0\.5|5\.0e-1)
**	fdivr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (divr_0p5_f16_m_tied1, svfloat16_t,
		z0 = svdivr_n_f16_m (p0, z0, 0.5),
		z0 = svdivr_m (p0, z0, 0.5))

/*
** divr_0p5_f16_m_untied: { xfail *-*-* }
**	fmov	(z[0-9]+\.h), #(?:0\.5|5\.0e-1)
**	movprfx	z0, z1
**	fdivr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (divr_0p5_f16_m_untied, svfloat16_t,
		z0 = svdivr_n_f16_m (p0, z1, 0.5),
		z0 = svdivr_m (p0, z1, 0.5))

/*
** divr_f16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	fdivr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (divr_f16_z_tied1, svfloat16_t,
		z0 = svdivr_f16_z (p0, z0, z1),
		z0 = svdivr_z (p0, z0, z1))

/*
** divr_f16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	fdiv	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (divr_f16_z_tied2, svfloat16_t,
		z0 = svdivr_f16_z (p0, z1, z0),
		z0 = svdivr_z (p0, z1, z0))

/*
** divr_f16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	fdivr	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0\.h, p0/z, z2\.h
**	fdiv	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (divr_f16_z_untied, svfloat16_t,
		z0 = svdivr_f16_z (p0, z1, z2),
		z0 = svdivr_z (p0, z1, z2))

/*
** divr_h4_f16_z_tied1:
**	mov	(z[0-9]+\.h), h4
**	movprfx	z0\.h, p0/z, z0\.h
**	fdivr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (divr_h4_f16_z_tied1, svfloat16_t, __fp16,
		 z0 = svdivr_n_f16_z (p0, z0, d4),
		 z0 = svdivr_z (p0, z0, d4))

/*
** divr_h4_f16_z_untied:
**	mov	(z[0-9]+\.h), h4
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	fdivr	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	fdiv	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZD (divr_h4_f16_z_untied, svfloat16_t, __fp16,
		 z0 = svdivr_n_f16_z (p0, z1, d4),
		 z0 = svdivr_z (p0, z1, d4))

/*
** divr_1_f16_z:
**	fmov	(z[0-9]+\.h), #1\.0(?:e\+0)?
**	movprfx	z0\.h, p0/z, z0\.h
**	fdivr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (divr_1_f16_z, svfloat16_t,
		z0 = svdivr_n_f16_z (p0, z0, 1),
		z0 = svdivr_z (p0, z0, 1))

/*
** divr_0p5_f16_z_tied1:
**	fmov	(z[0-9]+\.h), #(?:0\.5|5\.0e-1)
**	movprfx	z0\.h, p0/z, z0\.h
**	fdivr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (divr_0p5_f16_z_tied1, svfloat16_t,
		z0 = svdivr_n_f16_z (p0, z0, 0.5),
		z0 = svdivr_z (p0, z0, 0.5))

/*
** divr_0p5_f16_z_untied:
**	fmov	(z[0-9]+\.h), #(?:0\.5|5\.0e-1)
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	fdivr	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	fdiv	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (divr_0p5_f16_z_untied, svfloat16_t,
		z0 = svdivr_n_f16_z (p0, z1, 0.5),
		z0 = svdivr_z (p0, z1, 0.5))

/*
** divr_f16_x_tied1:
**	fdivr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (divr_f16_x_tied1, svfloat16_t,
		z0 = svdivr_f16_x (p0, z0, z1),
		z0 = svdivr_x (p0, z0, z1))

/*
** divr_f16_x_tied2:
**	fdiv	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (divr_f16_x_tied2, svfloat16_t,
		z0 = svdivr_f16_x (p0, z1, z0),
		z0 = svdivr_x (p0, z1, z0))

/*
** divr_f16_x_untied:
** (
**	movprfx	z0, z1
**	fdivr	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0, z2
**	fdiv	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (divr_f16_x_untied, svfloat16_t,
		z0 = svdivr_f16_x (p0, z1, z2),
		z0 = svdivr_x (p0, z1, z2))

/*
** divr_h4_f16_x_tied1:
**	mov	(z[0-9]+\.h), h4
**	fdivr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (divr_h4_f16_x_tied1, svfloat16_t, __fp16,
		 z0 = svdivr_n_f16_x (p0, z0, d4),
		 z0 = svdivr_x (p0, z0, d4))

/*
** divr_h4_f16_x_untied:
**	mov	z0\.h, h4
**	fdiv	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZD (divr_h4_f16_x_untied, svfloat16_t, __fp16,
		 z0 = svdivr_n_f16_x (p0, z1, d4),
		 z0 = svdivr_x (p0, z1, d4))

/*
** divr_1_f16_x_tied1:
**	fmov	(z[0-9]+\.h), #1\.0(?:e\+0)?
**	fdivr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (divr_1_f16_x_tied1, svfloat16_t,
		z0 = svdivr_n_f16_x (p0, z0, 1),
		z0 = svdivr_x (p0, z0, 1))

/*
** divr_1_f16_x_untied:
**	fmov	z0\.h, #1\.0(?:e\+0)?
**	fdiv	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (divr_1_f16_x_untied, svfloat16_t,
		z0 = svdivr_n_f16_x (p0, z1, 1),
		z0 = svdivr_x (p0, z1, 1))

/*
** ptrue_divr_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_divr_f16_x_tied1, svfloat16_t,
		z0 = svdivr_f16_x (svptrue_b16 (), z0, z1),
		z0 = svdivr_x (svptrue_b16 (), z0, z1))

/*
** ptrue_divr_f16_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_divr_f16_x_tied2, svfloat16_t,
		z0 = svdivr_f16_x (svptrue_b16 (), z1, z0),
		z0 = svdivr_x (svptrue_b16 (), z1, z0))

/*
** ptrue_divr_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_divr_f16_x_untied, svfloat16_t,
		z0 = svdivr_f16_x (svptrue_b16 (), z1, z2),
		z0 = svdivr_x (svptrue_b16 (), z1, z2))

/*
** ptrue_divr_1_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_divr_1_f16_x_tied1, svfloat16_t,
		z0 = svdivr_n_f16_x (svptrue_b16 (), z0, 1),
		z0 = svdivr_x (svptrue_b16 (), z0, 1))

/*
** ptrue_divr_1_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_divr_1_f16_x_untied, svfloat16_t,
		z0 = svdivr_n_f16_x (svptrue_b16 (), z1, 1),
		z0 = svdivr_x (svptrue_b16 (), z1, 1))
