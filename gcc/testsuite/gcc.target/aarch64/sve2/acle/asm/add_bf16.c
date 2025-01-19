/* { dg-do assemble { target aarch64_asm_sve-b16b16_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve-b16b16_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve-b16b16"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** add_bf16_m_tied1:
**	bfadd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (add_bf16_m_tied1, svbfloat16_t,
		z0 = svadd_bf16_m (p0, z0, z1),
		z0 = svadd_m (p0, z0, z1))

/*
** add_bf16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	bfadd	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (add_bf16_m_tied2, svbfloat16_t,
		z0 = svadd_bf16_m (p0, z1, z0),
		z0 = svadd_m (p0, z1, z0))

/*
** add_bf16_m_untied:
**	movprfx	z0, z1
**	bfadd	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (add_bf16_m_untied, svbfloat16_t,
		z0 = svadd_bf16_m (p0, z1, z2),
		z0 = svadd_m (p0, z1, z2))

/*
** add_h4_bf16_m_tied1:
**	mov	(z[0-9]+\.h), h4
**	bfadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (add_h4_bf16_m_tied1, svbfloat16_t, __bf16,
		 z0 = svadd_n_bf16_m (p0, z0, d4),
		 z0 = svadd_m (p0, z0, d4))

/*
** add_h4_bf16_m_untied:
**	mov	(z[0-9]+\.h), h4
**	movprfx	z0, z1
**	bfadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (add_h4_bf16_m_untied, svbfloat16_t, __bf16,
		 z0 = svadd_n_bf16_m (p0, z1, d4),
		 z0 = svadd_m (p0, z1, d4))

/*
** add_1_bf16_m:
**	fmov	(z[0-9]+\.h), #1\.875(?:e\+0)?
**	bfadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (add_1_bf16_m, svbfloat16_t,
		z0 = svadd_n_bf16_m (p0, z0, 1),
		z0 = svadd_m (p0, z0, 1))

/*
** add_0p5_bf16_m:
**	fmov	(z[0-9]+\.h), #1\.75(?:e\+0)?
**	bfadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (add_0p5_bf16_m, svbfloat16_t,
		z0 = svadd_n_bf16_m (p0, z0, (bfloat16_t) (0.5)),
		z0 = svadd_m (p0, z0, (bfloat16_t) (0.5)))

/*
** add_m1_bf16_m:
**	fmov	(z[0-9]+\.h), #-1\.875(?:e\+0)?
**	bfadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (add_m1_bf16_m, svbfloat16_t,
		z0 = svadd_n_bf16_m (p0, z0, -1),
		z0 = svadd_m (p0, z0, -1))

/*
** add_m0p5_bf16_m:
**	fmov	(z[0-9]+\.h), #-1\.75(?:e\+0)?
**	bfadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (add_m0p5_bf16_m, svbfloat16_t,
		z0 = svadd_n_bf16_m (p0, z0, (bfloat16_t) (-0.5)),
		z0 = svadd_m (p0, z0, (bfloat16_t) (-0.5)))

/*
** add_m2_bf16_m_tied1:
**	fmov	(z[0-9]+\.h), #-2\.0(?:e\+0)?
**	bfadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (add_m2_bf16_m_tied1, svbfloat16_t,
		z0 = svadd_n_bf16_m (p0, z0, -2),
		z0 = svadd_m (p0, z0, -2))

/*
** add_m2_bf16_m_untied:
**	fmov	(z[0-9]+\.h), #-2\.0(?:e\+0)?
**	movprfx	z0, z1
**	bfadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (add_m2_bf16_m_untied, svbfloat16_t,
		z0 = svadd_n_bf16_m (p0, z1, -2),
		z0 = svadd_m (p0, z1, -2))

/*
** add_bf16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	bfadd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (add_bf16_z_tied1, svbfloat16_t,
		z0 = svadd_bf16_z (p0, z0, z1),
		z0 = svadd_z (p0, z0, z1))

/*
** add_bf16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	bfadd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (add_bf16_z_tied2, svbfloat16_t,
		z0 = svadd_bf16_z (p0, z1, z0),
		z0 = svadd_z (p0, z1, z0))

/*
** add_bf16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	bfadd	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0\.h, p0/z, z2\.h
**	bfadd	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (add_bf16_z_untied, svbfloat16_t,
		z0 = svadd_bf16_z (p0, z1, z2),
		z0 = svadd_z (p0, z1, z2))

/*
** add_h4_bf16_z_tied1:
**	mov	(z[0-9]+\.h), h4
**	movprfx	z0\.h, p0/z, z0\.h
**	bfadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (add_h4_bf16_z_tied1, svbfloat16_t, __bf16,
		 z0 = svadd_n_bf16_z (p0, z0, d4),
		 z0 = svadd_z (p0, z0, d4))

/*
** add_h4_bf16_z_untied:
**	mov	(z[0-9]+\.h), h4
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	bfadd	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	bfadd	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZD (add_h4_bf16_z_untied, svbfloat16_t, __bf16,
		 z0 = svadd_n_bf16_z (p0, z1, d4),
		 z0 = svadd_z (p0, z1, d4))

/*
** add_1_bf16_z:
**	fmov	(z[0-9]+\.h), #1\.875(?:e\+0)?
**	movprfx	z0\.h, p0/z, z0\.h
**	bfadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (add_1_bf16_z, svbfloat16_t,
		z0 = svadd_n_bf16_z (p0, z0, 1),
		z0 = svadd_z (p0, z0, 1))

/*
** add_bf16_x_tied1:
**	bfadd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (add_bf16_x_tied1, svbfloat16_t,
		z0 = svadd_bf16_x (p0, z0, z1),
		z0 = svadd_x (p0, z0, z1))

/*
** add_bf16_x_tied2:
**	bfadd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (add_bf16_x_tied2, svbfloat16_t,
		z0 = svadd_bf16_x (p0, z1, z0),
		z0 = svadd_x (p0, z1, z0))

/*
** add_bf16_x_untied:
** (
**	movprfx	z0, z1
**	bfadd	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0, z2
**	bfadd	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (add_bf16_x_untied, svbfloat16_t,
		z0 = svadd_bf16_x (p0, z1, z2),
		z0 = svadd_x (p0, z1, z2))

/*
** add_h4_bf16_x_tied1:
**	mov	(z[0-9]+\.h), h4
**	bfadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZD (add_h4_bf16_x_tied1, svbfloat16_t, __bf16,
		 z0 = svadd_n_bf16_x (p0, z0, d4),
		 z0 = svadd_x (p0, z0, d4))

/*
** add_h4_bf16_x_untied:
**	mov	z0\.h, h4
**	bfadd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZD (add_h4_bf16_x_untied, svbfloat16_t, __bf16,
		 z0 = svadd_n_bf16_x (p0, z1, d4),
		 z0 = svadd_x (p0, z1, d4))

/*
** add_1_bf16_x_tied1:
**	fmov	(z[0-9]+\.h), #1\.875(?:e\+0)?
**	bfadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (add_1_bf16_x_tied1, svbfloat16_t,
		z0 = svadd_n_bf16_x (p0, z0, 1),
		z0 = svadd_x (p0, z0, 1))

/*
** add_1_bf16_x_untied:
**	fmov	z0\.h, #1\.875(?:e\+0)?
**	bfadd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (add_1_bf16_x_untied, svbfloat16_t,
		z0 = svadd_n_bf16_x (p0, z1, 1),
		z0 = svadd_x (p0, z1, 1))

/*
** ptrue_add_bf16_x_tied1:
**	bfadd	z0\.h, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_UNIFORM_Z (ptrue_add_bf16_x_tied1, svbfloat16_t,
		z0 = svadd_bf16_x (svptrue_b16 (), z0, z1),
		z0 = svadd_x (svptrue_b16 (), z0, z1))

/*
** ptrue_add_bf16_x_tied2:
**	bfadd	z0\.h, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_UNIFORM_Z (ptrue_add_bf16_x_tied2, svbfloat16_t,
		z0 = svadd_bf16_x (svptrue_b16 (), z1, z0),
		z0 = svadd_x (svptrue_b16 (), z1, z0))

/*
** ptrue_add_bf16_x_untied:
**	bfadd	z0\.h, (z1\.h, z2\.h|z2\.h, z1\.h)
**	ret
*/
TEST_UNIFORM_Z (ptrue_add_bf16_x_untied, svbfloat16_t,
		z0 = svadd_bf16_x (svptrue_b16 (), z1, z2),
		z0 = svadd_x (svptrue_b16 (), z1, z2))

/*
** ptrue_add_1_bf16_x_tied1:
**	fmov	(z[0-9]+\.h), #1\.875(?:e\+0)?
**	bfadd	z0\.h, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_UNIFORM_Z (ptrue_add_1_bf16_x_tied1, svbfloat16_t,
		z0 = svadd_n_bf16_x (svptrue_b16 (), z0, 1),
		z0 = svadd_x (svptrue_b16 (), z0, 1))

/*
** ptrue_add_1_bf16_x_untied:
**	fmov	(z[0-9]+\.h), #1\.875(?:e\+0)?
**	bfadd	z0\.h, (z1\.h, \1|\1, z1\.h)
**	ret
*/
TEST_UNIFORM_Z (ptrue_add_1_bf16_x_untied, svbfloat16_t,
		z0 = svadd_n_bf16_x (svptrue_b16 (), z1, 1),
		z0 = svadd_x (svptrue_b16 (), z1, 1))
