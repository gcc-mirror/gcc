/* { dg-do assemble { target aarch64_asm_sve-b16b16_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve-b16b16_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve-b16b16"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** mla_bf16_m_tied1:
**	bfmla	z0\.h, p0/m, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (mla_bf16_m_tied1, svbfloat16_t,
		z0 = svmla_bf16_m (p0, z0, z1, z2),
		z0 = svmla_m (p0, z0, z1, z2))

/*
** mla_bf16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	bfmla	z0\.h, p0/m, \1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (mla_bf16_m_tied2, svbfloat16_t,
		z0 = svmla_bf16_m (p0, z1, z0, z2),
		z0 = svmla_m (p0, z1, z0, z2))

/*
** mla_bf16_m_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	bfmla	z0\.h, p0/m, z2\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (mla_bf16_m_tied3, svbfloat16_t,
		z0 = svmla_bf16_m (p0, z1, z2, z0),
		z0 = svmla_m (p0, z1, z2, z0))

/*
** mla_bf16_m_untied:
**	movprfx	z0, z1
**	bfmla	z0\.h, p0/m, z2\.h, z3\.h
**	ret
*/
TEST_UNIFORM_Z (mla_bf16_m_untied, svbfloat16_t,
		z0 = svmla_bf16_m (p0, z1, z2, z3),
		z0 = svmla_m (p0, z1, z2, z3))

/*
** mla_h4_bf16_m_tied1:
**	mov	(z[0-9]+\.h), h4
**	bfmla	z0\.h, p0/m, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (mla_h4_bf16_m_tied1, svbfloat16_t, __bf16,
		 z0 = svmla_n_bf16_m (p0, z0, z1, d4),
		 z0 = svmla_m (p0, z0, z1, d4))

/*
** mla_h4_bf16_m_untied:
**	mov	(z[0-9]+\.h), h4
**	movprfx	z0, z1
**	bfmla	z0\.h, p0/m, z2\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (mla_h4_bf16_m_untied, svbfloat16_t, __bf16,
		 z0 = svmla_n_bf16_m (p0, z1, z2, d4),
		 z0 = svmla_m (p0, z1, z2, d4))

/*
** mla_2_bf16_m_tied1:
**	fmov	(z[0-9]+\.h), #2\.0(?:e\+0)?
**	bfmla	z0\.h, p0/m, z1\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mla_2_bf16_m_tied1, svbfloat16_t,
		z0 = svmla_n_bf16_m (p0, z0, z1, 2),
		z0 = svmla_m (p0, z0, z1, 2))

/*
** mla_2_bf16_m_untied:
**	fmov	(z[0-9]+\.h), #2\.0(?:e\+0)?
**	movprfx	z0, z1
**	bfmla	z0\.h, p0/m, z2\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mla_2_bf16_m_untied, svbfloat16_t,
		z0 = svmla_n_bf16_m (p0, z1, z2, 2),
		z0 = svmla_m (p0, z1, z2, 2))

/*
** mla_bf16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	bfmla	z0\.h, p0/m, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (mla_bf16_z_tied1, svbfloat16_t,
		z0 = svmla_bf16_z (p0, z0, z1, z2),
		z0 = svmla_z (p0, z0, z1, z2))

/*
** mla_bf16_z_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, z1\.h
**	bfmla	z0\.h, p0/m, \1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (mla_bf16_z_tied2, svbfloat16_t,
		z0 = svmla_bf16_z (p0, z1, z0, z2),
		z0 = svmla_z (p0, z1, z0, z2))

/*
** mla_bf16_z_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, z1\.h
**	bfmla	z0\.h, p0/m, z2\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (mla_bf16_z_tied3, svbfloat16_t,
		z0 = svmla_bf16_z (p0, z1, z2, z0),
		z0 = svmla_z (p0, z1, z2, z0))

/*
** mla_bf16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	bfmla	z0\.h, p0/m, z2\.h, z3\.h
**	ret
*/
TEST_UNIFORM_Z (mla_bf16_z_untied, svbfloat16_t,
		z0 = svmla_bf16_z (p0, z1, z2, z3),
		z0 = svmla_z (p0, z1, z2, z3))

/*
** mla_h4_bf16_z_tied1:
**	mov	(z[0-9]+\.h), h4
**	movprfx	z0\.h, p0/z, z0\.h
**	bfmla	z0\.h, p0/m, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (mla_h4_bf16_z_tied1, svbfloat16_t, __bf16,
		 z0 = svmla_n_bf16_z (p0, z0, z1, d4),
		 z0 = svmla_z (p0, z0, z1, d4))

/*
** mla_h4_bf16_z_tied2:
**	mov	(z[0-9]+\.h), h4
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, z1\.h
**	bfmla	z0\.h, p0/m, \2\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (mla_h4_bf16_z_tied2, svbfloat16_t, __bf16,
		 z0 = svmla_n_bf16_z (p0, z1, z0, d4),
		 z0 = svmla_z (p0, z1, z0, d4))

/*
** mla_h4_bf16_z_untied:
**	mov	(z[0-9]+\.h), h4
**	movprfx	z0\.h, p0/z, z1\.h
**	bfmla	z0\.h, p0/m, z2\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (mla_h4_bf16_z_untied, svbfloat16_t, __bf16,
		 z0 = svmla_n_bf16_z (p0, z1, z2, d4),
		 z0 = svmla_z (p0, z1, z2, d4))

/*
** mla_2_bf16_z_tied1:
**	fmov	(z[0-9]+\.h), #2\.0(?:e\+0)?
**	movprfx	z0\.h, p0/z, z0\.h
**	bfmla	z0\.h, p0/m, z1\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mla_2_bf16_z_tied1, svbfloat16_t,
		z0 = svmla_n_bf16_z (p0, z0, z1, 2),
		z0 = svmla_z (p0, z0, z1, 2))

/*
** mla_bf16_x_tied1:
**	bfmla	z0\.h, p0/m, z1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (mla_bf16_x_tied1, svbfloat16_t,
		z0 = svmla_bf16_x (p0, z0, z1, z2),
		z0 = svmla_x (p0, z0, z1, z2))

/*
** mla_bf16_x_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	bfmla	z0\.h, p0/m, \1\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (mla_bf16_x_tied2, svbfloat16_t,
		z0 = svmla_bf16_x (p0, z1, z0, z2),
		z0 = svmla_x (p0, z1, z0, z2))

/*
** mla_bf16_x_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	bfmla	z0\.h, p0/m, z2\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (mla_bf16_x_tied3, svbfloat16_t,
		z0 = svmla_bf16_x (p0, z1, z2, z0),
		z0 = svmla_x (p0, z1, z2, z0))

/*
** mla_bf16_x_untied:
**	movprfx	z0, z1
**	bfmla	z0\.h, p0/m, z2\.h, z3\.h
**	ret
*/
TEST_UNIFORM_Z (mla_bf16_x_untied, svbfloat16_t,
		z0 = svmla_bf16_x (p0, z1, z2, z3),
		z0 = svmla_x (p0, z1, z2, z3))

/*
** mla_h4_bf16_x_tied1:
**	mov	(z[0-9]+\.h), h4
**	bfmla	z0\.h, p0/m, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (mla_h4_bf16_x_tied1, svbfloat16_t, __bf16,
		 z0 = svmla_n_bf16_x (p0, z0, z1, d4),
		 z0 = svmla_x (p0, z0, z1, d4))

/*
** mla_h4_bf16_x_tied2:
**	mov	(z[0-9]+\.h), h4
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	bfmla	z0\.h, p0/m, \2\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (mla_h4_bf16_x_tied2, svbfloat16_t, __bf16,
		 z0 = svmla_n_bf16_x (p0, z1, z0, d4),
		 z0 = svmla_x (p0, z1, z0, d4))

/*
** mla_h4_bf16_x_untied:
**	mov	(z[0-9]+\.h), h4
**	movprfx	z0, z1
**	bfmla	z0\.h, p0/m, z2\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (mla_h4_bf16_x_untied, svbfloat16_t, __bf16,
		 z0 = svmla_n_bf16_x (p0, z1, z2, d4),
		 z0 = svmla_x (p0, z1, z2, d4))

/*
** mla_2_bf16_x_tied1:
**	fmov	(z[0-9]+\.h), #2\.0(?:e\+0)?
**	bfmla	z0\.h, p0/m, z1\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mla_2_bf16_x_tied1, svbfloat16_t,
		z0 = svmla_n_bf16_x (p0, z0, z1, 2),
		z0 = svmla_x (p0, z0, z1, 2))

/*
** ptrue_mla_bf16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_mla_bf16_x_tied1, svbfloat16_t,
		z0 = svmla_bf16_x (svptrue_b16 (), z0, z1, z2),
		z0 = svmla_x (svptrue_b16 (), z0, z1, z2))

/*
** ptrue_mla_bf16_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_mla_bf16_x_tied2, svbfloat16_t,
		z0 = svmla_bf16_x (svptrue_b16 (), z1, z0, z2),
		z0 = svmla_x (svptrue_b16 (), z1, z0, z2))

/*
** ptrue_mla_bf16_x_tied3:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_mla_bf16_x_tied3, svbfloat16_t,
		z0 = svmla_bf16_x (svptrue_b16 (), z1, z2, z0),
		z0 = svmla_x (svptrue_b16 (), z1, z2, z0))

/*
** ptrue_mla_bf16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_mla_bf16_x_untied, svbfloat16_t,
		z0 = svmla_bf16_x (svptrue_b16 (), z1, z2, z3),
		z0 = svmla_x (svptrue_b16 (), z1, z2, z3))

/*
** ptrue_mla_2_bf16_x_tied1:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_mla_2_bf16_x_tied1, svbfloat16_t,
		z0 = svmla_n_bf16_x (svptrue_b16 (), z0, z1, 2),
		z0 = svmla_x (svptrue_b16 (), z0, z1, 2))

/*
** ptrue_mla_2_bf16_x_tied2:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_mla_2_bf16_x_tied2, svbfloat16_t,
		z0 = svmla_n_bf16_x (svptrue_b16 (), z1, z0, 2),
		z0 = svmla_x (svptrue_b16 (), z1, z0, 2))

/*
** ptrue_mla_2_bf16_x_untied:
**	...
**	ptrue	p[0-9]+\.b[^\n]*
**	...
**	ret
*/
TEST_UNIFORM_Z (ptrue_mla_2_bf16_x_untied, svbfloat16_t,
		z0 = svmla_n_bf16_x (svptrue_b16 (), z1, z2, 2),
		z0 = svmla_x (svptrue_b16 (), z1, z2, 2))
