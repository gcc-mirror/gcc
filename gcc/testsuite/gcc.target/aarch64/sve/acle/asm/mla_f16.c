/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mla_f16_m_tied1:
**	fmla	z0\.h, p0/m, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (mla_f16_m_tied1, svfloat16_t,
		z0 = svmla_f16_m (p0, z0, z1, z2),
		z0 = svmla_m (p0, z0, z1, z2))

/*
** mla_f16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fmla	z0\.h, p0/m, \1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (mla_f16_m_tied2, svfloat16_t,
		z0 = svmla_f16_m (p0, z1, z0, z2),
		z0 = svmla_m (p0, z1, z0, z2))

/*
** mla_f16_m_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	fmla	z0\.h, p0/m, z2\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (mla_f16_m_tied3, svfloat16_t,
		z0 = svmla_f16_m (p0, z1, z2, z0),
		z0 = svmla_m (p0, z1, z2, z0))

/*
** mla_f16_m_untied:
**	movprfx	z0, z1
**	fmla	z0\.h, p0/m, z2\.h, z3\.h
**	ret
*/
TEST_UNIFORM_Z (mla_f16_m_untied, svfloat16_t,
		z0 = svmla_f16_m (p0, z1, z2, z3),
		z0 = svmla_m (p0, z1, z2, z3))

/*
** mla_h4_f16_m_tied1:
**	mov	(z[0-9]+\.h), h4
**	fmla	z0\.h, p0/m, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (mla_h4_f16_m_tied1, svfloat16_t, __fp16,
		 z0 = svmla_n_f16_m (p0, z0, z1, d4),
		 z0 = svmla_m (p0, z0, z1, d4))

/*
** mla_h4_f16_m_untied:
**	mov	(z[0-9]+\.h), h4
**	movprfx	z0, z1
**	fmla	z0\.h, p0/m, z2\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (mla_h4_f16_m_untied, svfloat16_t, __fp16,
		 z0 = svmla_n_f16_m (p0, z1, z2, d4),
		 z0 = svmla_m (p0, z1, z2, d4))

/*
** mla_2_f16_m_tied1:
**	fmov	(z[0-9]+\.h), #2\.0(?:e\+0)?
**	fmla	z0\.h, p0/m, z1\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mla_2_f16_m_tied1, svfloat16_t,
		z0 = svmla_n_f16_m (p0, z0, z1, 2),
		z0 = svmla_m (p0, z0, z1, 2))

/*
** mla_2_f16_m_untied: { xfail *-*-* }
**	fmov	(z[0-9]+\.h), #2\.0(?:e\+0)?
**	movprfx	z0, z1
**	fmla	z0\.h, p0/m, z2\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mla_2_f16_m_untied, svfloat16_t,
		z0 = svmla_n_f16_m (p0, z1, z2, 2),
		z0 = svmla_m (p0, z1, z2, 2))

/*
** mla_f16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	fmla	z0\.h, p0/m, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (mla_f16_z_tied1, svfloat16_t,
		z0 = svmla_f16_z (p0, z0, z1, z2),
		z0 = svmla_z (p0, z0, z1, z2))

/*
** mla_f16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	fmad	z0\.h, p0/m, z2\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (mla_f16_z_tied2, svfloat16_t,
		z0 = svmla_f16_z (p0, z1, z0, z2),
		z0 = svmla_z (p0, z1, z0, z2))

/*
** mla_f16_z_tied3:
**	movprfx	z0\.h, p0/z, z0\.h
**	fmad	z0\.h, p0/m, z2\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (mla_f16_z_tied3, svfloat16_t,
		z0 = svmla_f16_z (p0, z1, z2, z0),
		z0 = svmla_z (p0, z1, z2, z0))

/*
** mla_f16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	fmla	z0\.h, p0/m, z2\.h, z3\.h
** |
**	movprfx	z0\.h, p0/z, z2\.h
**	fmad	z0\.h, p0/m, z3\.h, z1\.h
** |
**	movprfx	z0\.h, p0/z, z3\.h
**	fmad	z0\.h, p0/m, z2\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (mla_f16_z_untied, svfloat16_t,
		z0 = svmla_f16_z (p0, z1, z2, z3),
		z0 = svmla_z (p0, z1, z2, z3))

/*
** mla_h4_f16_z_tied1:
**	mov	(z[0-9]+\.h), h4
**	movprfx	z0\.h, p0/z, z0\.h
**	fmla	z0\.h, p0/m, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (mla_h4_f16_z_tied1, svfloat16_t, __fp16,
		 z0 = svmla_n_f16_z (p0, z0, z1, d4),
		 z0 = svmla_z (p0, z0, z1, d4))

/*
** mla_h4_f16_z_tied2:
**	mov	(z[0-9]+\.h), h4
**	movprfx	z0\.h, p0/z, z0\.h
**	fmad	z0\.h, p0/m, \1, z1\.h
**	ret
*/
TEST_UNIFORM_ZD (mla_h4_f16_z_tied2, svfloat16_t, __fp16,
		 z0 = svmla_n_f16_z (p0, z1, z0, d4),
		 z0 = svmla_z (p0, z1, z0, d4))

/*
** mla_h4_f16_z_untied:
**	mov	(z[0-9]+\.h), h4
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	fmla	z0\.h, p0/m, z2\.h, \1
** |
**	movprfx	z0\.h, p0/z, z2\.h
**	fmad	z0\.h, p0/m, \1, z1\.h
** |
**	movprfx	z0\.h, p0/z, \1
**	fmad	z0\.h, p0/m, z2\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZD (mla_h4_f16_z_untied, svfloat16_t, __fp16,
		 z0 = svmla_n_f16_z (p0, z1, z2, d4),
		 z0 = svmla_z (p0, z1, z2, d4))

/*
** mla_2_f16_z_tied1:
**	fmov	(z[0-9]+\.h), #2\.0(?:e\+0)?
**	movprfx	z0\.h, p0/z, z0\.h
**	fmla	z0\.h, p0/m, z1\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mla_2_f16_z_tied1, svfloat16_t,
		z0 = svmla_n_f16_z (p0, z0, z1, 2),
		z0 = svmla_z (p0, z0, z1, 2))

/*
** mla_2_f16_z_tied2:
**	fmov	(z[0-9]+\.h), #2\.0(?:e\+0)?
**	movprfx	z0\.h, p0/z, z0\.h
**	fmad	z0\.h, p0/m, \1, z1\.h
**	ret
*/
TEST_UNIFORM_Z (mla_2_f16_z_tied2, svfloat16_t,
		z0 = svmla_n_f16_z (p0, z1, z0, 2),
		z0 = svmla_z (p0, z1, z0, 2))

/*
** mla_2_f16_z_untied:
**	fmov	(z[0-9]+\.h), #2\.0(?:e\+0)?
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	fmla	z0\.h, p0/m, z2\.h, \1
** |
**	movprfx	z0\.h, p0/z, z2\.h
**	fmad	z0\.h, p0/m, \1, z1\.h
** |
**	movprfx	z0\.h, p0/z, \1
**	fmad	z0\.h, p0/m, z2\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (mla_2_f16_z_untied, svfloat16_t,
		z0 = svmla_n_f16_z (p0, z1, z2, 2),
		z0 = svmla_z (p0, z1, z2, 2))

/*
** mla_f16_x_tied1:
**	fmla	z0\.h, p0/m, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (mla_f16_x_tied1, svfloat16_t,
		z0 = svmla_f16_x (p0, z0, z1, z2),
		z0 = svmla_x (p0, z0, z1, z2))

/*
** mla_f16_x_tied2:
**	fmad	z0\.h, p0/m, z2\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (mla_f16_x_tied2, svfloat16_t,
		z0 = svmla_f16_x (p0, z1, z0, z2),
		z0 = svmla_x (p0, z1, z0, z2))

/*
** mla_f16_x_tied3:
**	fmad	z0\.h, p0/m, z2\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (mla_f16_x_tied3, svfloat16_t,
		z0 = svmla_f16_x (p0, z1, z2, z0),
		z0 = svmla_x (p0, z1, z2, z0))

/*
** mla_f16_x_untied:
** (
**	movprfx	z0, z1
**	fmla	z0\.h, p0/m, z2\.h, z3\.h
** |
**	movprfx	z0, z2
**	fmad	z0\.h, p0/m, z3\.h, z1\.h
** |
**	movprfx	z0, z3
**	fmad	z0\.h, p0/m, z2\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (mla_f16_x_untied, svfloat16_t,
		z0 = svmla_f16_x (p0, z1, z2, z3),
		z0 = svmla_x (p0, z1, z2, z3))

/*
** mla_h4_f16_x_tied1:
**	mov	(z[0-9]+\.h), h4
**	fmla	z0\.h, p0/m, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (mla_h4_f16_x_tied1, svfloat16_t, __fp16,
		 z0 = svmla_n_f16_x (p0, z0, z1, d4),
		 z0 = svmla_x (p0, z0, z1, d4))

/*
** mla_h4_f16_x_tied2:
**	mov	(z[0-9]+\.h), h4
**	fmad	z0\.h, p0/m, \1, z1\.h
**	ret
*/
TEST_UNIFORM_ZD (mla_h4_f16_x_tied2, svfloat16_t, __fp16,
		 z0 = svmla_n_f16_x (p0, z1, z0, d4),
		 z0 = svmla_x (p0, z1, z0, d4))

/*
** mla_h4_f16_x_untied: { xfail *-*-* }
**	mov	z0\.h, h4
**	fmad	z0\.h, p0/m, z2\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZD (mla_h4_f16_x_untied, svfloat16_t, __fp16,
		 z0 = svmla_n_f16_x (p0, z1, z2, d4),
		 z0 = svmla_x (p0, z1, z2, d4))

/*
** mla_2_f16_x_tied1:
**	fmov	(z[0-9]+\.h), #2\.0(?:e\+0)?
**	fmla	z0\.h, p0/m, z1\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mla_2_f16_x_tied1, svfloat16_t,
		z0 = svmla_n_f16_x (p0, z0, z1, 2),
		z0 = svmla_x (p0, z0, z1, 2))

/*
** mla_2_f16_x_tied2:
**	fmov	(z[0-9]+\.h), #2\.0(?:e\+0)?
**	fmad	z0\.h, p0/m, \1, z1\.h
**	ret
*/
TEST_UNIFORM_Z (mla_2_f16_x_tied2, svfloat16_t,
		z0 = svmla_n_f16_x (p0, z1, z0, 2),
		z0 = svmla_x (p0, z1, z0, 2))

/*
** mla_2_f16_x_untied:
**	fmov	z0\.h, #2\.0(?:e\+0)?
**	fmad	z0\.h, p0/m, z2\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (mla_2_f16_x_untied, svfloat16_t,
		z0 = svmla_n_f16_x (p0, z1, z2, 2),
		z0 = svmla_x (p0, z1, z2, 2))

/*
** ptrue_mla_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_mla_f16_x_tied1, svfloat16_t,
		z0 = svmla_f16_x (svptrue_b16 (), z0, z1, z2),
		z0 = svmla_x (svptrue_b16 (), z0, z1, z2))

/*
** ptrue_mla_f16_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_mla_f16_x_tied2, svfloat16_t,
		z0 = svmla_f16_x (svptrue_b16 (), z1, z0, z2),
		z0 = svmla_x (svptrue_b16 (), z1, z0, z2))

/*
** ptrue_mla_f16_x_tied3:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_mla_f16_x_tied3, svfloat16_t,
		z0 = svmla_f16_x (svptrue_b16 (), z1, z2, z0),
		z0 = svmla_x (svptrue_b16 (), z1, z2, z0))

/*
** ptrue_mla_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_mla_f16_x_untied, svfloat16_t,
		z0 = svmla_f16_x (svptrue_b16 (), z1, z2, z3),
		z0 = svmla_x (svptrue_b16 (), z1, z2, z3))

/*
** ptrue_mla_2_f16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_mla_2_f16_x_tied1, svfloat16_t,
		z0 = svmla_n_f16_x (svptrue_b16 (), z0, z1, 2),
		z0 = svmla_x (svptrue_b16 (), z0, z1, 2))

/*
** ptrue_mla_2_f16_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_mla_2_f16_x_tied2, svfloat16_t,
		z0 = svmla_n_f16_x (svptrue_b16 (), z1, z0, 2),
		z0 = svmla_x (svptrue_b16 (), z1, z0, 2))

/*
** ptrue_mla_2_f16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_mla_2_f16_x_untied, svfloat16_t,
		z0 = svmla_n_f16_x (svptrue_b16 (), z1, z2, 2),
		z0 = svmla_x (svptrue_b16 (), z1, z2, 2))
