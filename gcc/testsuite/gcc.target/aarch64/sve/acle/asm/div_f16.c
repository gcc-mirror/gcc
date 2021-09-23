/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** div_f16_m_tied1:
**	fdiv	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (div_f16_m_tied1, svfloat16_t,
		z0 = svdiv_f16_m (p0, z0, z1),
		z0 = svdiv_m (p0, z0, z1))

/*
** div_f16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fdiv	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (div_f16_m_tied2, svfloat16_t,
		z0 = svdiv_f16_m (p0, z1, z0),
		z0 = svdiv_m (p0, z1, z0))

/*
** div_f16_m_untied:
**	movprfx	z0, z1
**	fdiv	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (div_f16_m_untied, svfloat16_t,
		z0 = svdiv_f16_m (p0, z1, z2),
		z0 = svdiv_m (p0, z1, z2))

/*
** div_h4_f16_m_tied1:
**	mov	(z[0-9]+\.h), h4
**	fdiv	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (div_h4_f16_m_tied1, svfloat16_t, __fp16,
		 z0 = svdiv_n_f16_m (p0, z0, d4),
		 z0 = svdiv_m (p0, z0, d4))

/*
** div_h4_f16_m_untied:
**	mov	(z[0-9]+\.h), h4
**	movprfx	z0, z1
**	fdiv	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (div_h4_f16_m_untied, svfloat16_t, __fp16,
		 z0 = svdiv_n_f16_m (p0, z1, d4),
		 z0 = svdiv_m (p0, z1, d4))

/*
** div_1_f16_m_tied1:
**	fmov	(z[0-9]+\.h), #1\.0(?:e\+0)?
**	fdiv	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (div_1_f16_m_tied1, svfloat16_t,
		z0 = svdiv_n_f16_m (p0, z0, 1),
		z0 = svdiv_m (p0, z0, 1))

/*
** div_1_f16_m_untied: { xfail *-*-* }
**	fmov	(z[0-9]+\.h), #1\.0(?:e\+0)?
**	movprfx	z0, z1
**	fdiv	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (div_1_f16_m_untied, svfloat16_t,
		z0 = svdiv_n_f16_m (p0, z1, 1),
		z0 = svdiv_m (p0, z1, 1))

/*
** div_f16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	fdiv	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (div_f16_z_tied1, svfloat16_t,
		z0 = svdiv_f16_z (p0, z0, z1),
		z0 = svdiv_z (p0, z0, z1))

/*
** div_f16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	fdivr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (div_f16_z_tied2, svfloat16_t,
		z0 = svdiv_f16_z (p0, z1, z0),
		z0 = svdiv_z (p0, z1, z0))

/*
** div_f16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	fdiv	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0\.h, p0/z, z2\.h
**	fdivr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (div_f16_z_untied, svfloat16_t,
		z0 = svdiv_f16_z (p0, z1, z2),
		z0 = svdiv_z (p0, z1, z2))

/*
** div_h4_f16_z_tied1:
**	mov	(z[0-9]+\.h), h4
**	movprfx	z0\.h, p0/z, z0\.h
**	fdiv	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (div_h4_f16_z_tied1, svfloat16_t, __fp16,
		 z0 = svdiv_n_f16_z (p0, z0, d4),
		 z0 = svdiv_z (p0, z0, d4))

/*
** div_h4_f16_z_untied:
**	mov	(z[0-9]+\.h), h4
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	fdiv	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	fdivr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZD (div_h4_f16_z_untied, svfloat16_t, __fp16,
		 z0 = svdiv_n_f16_z (p0, z1, d4),
		 z0 = svdiv_z (p0, z1, d4))

/*
** div_1_f16_z_tied1:
**	fmov	(z[0-9]+\.h), #1\.0(?:e\+0)?
**	movprfx	z0\.h, p0/z, z0\.h
**	fdiv	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (div_1_f16_z_tied1, svfloat16_t,
		z0 = svdiv_n_f16_z (p0, z0, 1),
		z0 = svdiv_z (p0, z0, 1))

/*
** div_1_f16_z_untied:
**	fmov	(z[0-9]+\.h), #1\.0(?:e\+0)?
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	fdiv	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	fdivr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (div_1_f16_z_untied, svfloat16_t,
		z0 = svdiv_n_f16_z (p0, z1, 1),
		z0 = svdiv_z (p0, z1, 1))

/*
** div_0p5_f16_z:
**	fmov	(z[0-9]+\.h), #(?:0\.5|5\.0e-1)
**	movprfx	z0\.h, p0/z, z0\.h
**	fdiv	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (div_0p5_f16_z, svfloat16_t,
		z0 = svdiv_n_f16_z (p0, z0, 0.5),
		z0 = svdiv_z (p0, z0, 0.5))

/*
** div_f16_x_tied1:
**	fdiv	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (div_f16_x_tied1, svfloat16_t,
		z0 = svdiv_f16_x (p0, z0, z1),
		z0 = svdiv_x (p0, z0, z1))

/*
** div_f16_x_tied2:
**	fdivr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (div_f16_x_tied2, svfloat16_t,
		z0 = svdiv_f16_x (p0, z1, z0),
		z0 = svdiv_x (p0, z1, z0))

/*
** div_f16_x_untied:
** (
**	movprfx	z0, z1
**	fdiv	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0, z2
**	fdivr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (div_f16_x_untied, svfloat16_t,
		z0 = svdiv_f16_x (p0, z1, z2),
		z0 = svdiv_x (p0, z1, z2))

/*
** div_h4_f16_x_tied1:
**	mov	(z[0-9]+\.h), h4
**	fdiv	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (div_h4_f16_x_tied1, svfloat16_t, __fp16,
		 z0 = svdiv_n_f16_x (p0, z0, d4),
		 z0 = svdiv_x (p0, z0, d4))

/*
** div_h4_f16_x_untied:
**	mov	z0\.h, h4
**	fdivr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZD (div_h4_f16_x_untied, svfloat16_t, __fp16,
		 z0 = svdiv_n_f16_x (p0, z1, d4),
		 z0 = svdiv_x (p0, z1, d4))

/*
** div_1_f16_x_tied1:
**	fmov	(z[0-9]+\.h), #1\.0(?:e\+0)?
**	fdiv	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (div_1_f16_x_tied1, svfloat16_t,
		z0 = svdiv_n_f16_x (p0, z0, 1),
		z0 = svdiv_x (p0, z0, 1))

/*
** div_1_f16_x_untied:
**	fmov	z0\.h, #1\.0(?:e\+0)?
**	fdivr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (div_1_f16_x_untied, svfloat16_t,
		z0 = svdiv_n_f16_x (p0, z1, 1),
		z0 = svdiv_x (p0, z1, 1))

/*
** ptrue_div_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_div_f16_x_tied1, svfloat16_t,
		z0 = svdiv_f16_x (svptrue_b16 (), z0, z1),
		z0 = svdiv_x (svptrue_b16 (), z0, z1))

/*
** ptrue_div_f16_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_div_f16_x_tied2, svfloat16_t,
		z0 = svdiv_f16_x (svptrue_b16 (), z1, z0),
		z0 = svdiv_x (svptrue_b16 (), z1, z0))

/*
** ptrue_div_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_div_f16_x_untied, svfloat16_t,
		z0 = svdiv_f16_x (svptrue_b16 (), z1, z2),
		z0 = svdiv_x (svptrue_b16 (), z1, z2))

/*
** ptrue_div_1_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_div_1_f16_x_tied1, svfloat16_t,
		z0 = svdiv_n_f16_x (svptrue_b16 (), z0, 1),
		z0 = svdiv_x (svptrue_b16 (), z0, 1))

/*
** ptrue_div_1_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_div_1_f16_x_untied, svfloat16_t,
		z0 = svdiv_n_f16_x (svptrue_b16 (), z1, 1),
		z0 = svdiv_x (svptrue_b16 (), z1, 1))
