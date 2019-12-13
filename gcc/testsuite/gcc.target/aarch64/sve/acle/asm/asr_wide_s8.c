/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** asr_wide_s8_m_tied1:
**	asr	z0\.b, p0/m, z0\.b, z4\.d
**	ret
*/
TEST_DUAL_Z (asr_wide_s8_m_tied1, svint8_t, svuint64_t,
	     z0 = svasr_wide_s8_m (p0, z0, z4),
	     z0 = svasr_wide_m (p0, z0, z4))

/*
** asr_wide_s8_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z4
**	asr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_DUAL_Z_REV (asr_wide_s8_m_tied2, svint8_t, svuint64_t,
		 z0_res = svasr_wide_s8_m (p0, z4, z0),
		 z0_res = svasr_wide_m (p0, z4, z0))

/*
** asr_wide_s8_m_untied:
**	movprfx	z0, z1
**	asr	z0\.b, p0/m, z0\.b, z4\.d
**	ret
*/
TEST_DUAL_Z (asr_wide_s8_m_untied, svint8_t, svuint64_t,
	     z0 = svasr_wide_s8_m (p0, z1, z4),
	     z0 = svasr_wide_m (p0, z1, z4))

/*
** asr_wide_x0_s8_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	asr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (asr_wide_x0_s8_m_tied1, svint8_t, uint64_t,
		 z0 = svasr_wide_n_s8_m (p0, z0, x0),
		 z0 = svasr_wide_m (p0, z0, x0))

/*
** asr_wide_x0_s8_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	asr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (asr_wide_x0_s8_m_untied, svint8_t, uint64_t,
		 z0 = svasr_wide_n_s8_m (p0, z1, x0),
		 z0 = svasr_wide_m (p0, z1, x0))

/*
** asr_wide_1_s8_m_tied1:
**	asr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (asr_wide_1_s8_m_tied1, svint8_t,
		z0 = svasr_wide_n_s8_m (p0, z0, 1),
		z0 = svasr_wide_m (p0, z0, 1))

/*
** asr_wide_1_s8_m_untied:
**	movprfx	z0, z1
**	asr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (asr_wide_1_s8_m_untied, svint8_t,
		z0 = svasr_wide_n_s8_m (p0, z1, 1),
		z0 = svasr_wide_m (p0, z1, 1))

/*
** asr_wide_7_s8_m_tied1:
**	asr	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (asr_wide_7_s8_m_tied1, svint8_t,
		z0 = svasr_wide_n_s8_m (p0, z0, 7),
		z0 = svasr_wide_m (p0, z0, 7))

/*
** asr_wide_7_s8_m_untied:
**	movprfx	z0, z1
**	asr	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (asr_wide_7_s8_m_untied, svint8_t,
		z0 = svasr_wide_n_s8_m (p0, z1, 7),
		z0 = svasr_wide_m (p0, z1, 7))

/*
** asr_wide_8_s8_m_tied1:
**	asr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (asr_wide_8_s8_m_tied1, svint8_t,
		z0 = svasr_wide_n_s8_m (p0, z0, 8),
		z0 = svasr_wide_m (p0, z0, 8))

/*
** asr_wide_8_s8_m_untied:
**	movprfx	z0, z1
**	asr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (asr_wide_8_s8_m_untied, svint8_t,
		z0 = svasr_wide_n_s8_m (p0, z1, 8),
		z0 = svasr_wide_m (p0, z1, 8))

/*
** asr_wide_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	asr	z0\.b, p0/m, z0\.b, z4\.d
**	ret
*/
TEST_DUAL_Z (asr_wide_s8_z_tied1, svint8_t, svuint64_t,
	     z0 = svasr_wide_s8_z (p0, z0, z4),
	     z0 = svasr_wide_z (p0, z0, z4))

/*
** asr_wide_s8_z_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.b, p0/z, z4\.b
**	asr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_DUAL_Z_REV (asr_wide_s8_z_tied2, svint8_t, svuint64_t,
		 z0_res = svasr_wide_s8_z (p0, z4, z0),
		 z0_res = svasr_wide_z (p0, z4, z0))

/*
** asr_wide_s8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	asr	z0\.b, p0/m, z0\.b, z4\.d
**	ret
*/
TEST_DUAL_Z (asr_wide_s8_z_untied, svint8_t, svuint64_t,
	     z0 = svasr_wide_s8_z (p0, z1, z4),
	     z0 = svasr_wide_z (p0, z1, z4))

/*
** asr_wide_x0_s8_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.b, p0/z, z0\.b
**	asr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (asr_wide_x0_s8_z_tied1, svint8_t, uint64_t,
		 z0 = svasr_wide_n_s8_z (p0, z0, x0),
		 z0 = svasr_wide_z (p0, z0, x0))

/*
** asr_wide_x0_s8_z_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.b, p0/z, z1\.b
**	asr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (asr_wide_x0_s8_z_untied, svint8_t, uint64_t,
		 z0 = svasr_wide_n_s8_z (p0, z1, x0),
		 z0 = svasr_wide_z (p0, z1, x0))

/*
** asr_wide_1_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	asr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (asr_wide_1_s8_z_tied1, svint8_t,
		z0 = svasr_wide_n_s8_z (p0, z0, 1),
		z0 = svasr_wide_z (p0, z0, 1))

/*
** asr_wide_1_s8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	asr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (asr_wide_1_s8_z_untied, svint8_t,
		z0 = svasr_wide_n_s8_z (p0, z1, 1),
		z0 = svasr_wide_z (p0, z1, 1))

/*
** asr_wide_7_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	asr	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (asr_wide_7_s8_z_tied1, svint8_t,
		z0 = svasr_wide_n_s8_z (p0, z0, 7),
		z0 = svasr_wide_z (p0, z0, 7))

/*
** asr_wide_7_s8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	asr	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (asr_wide_7_s8_z_untied, svint8_t,
		z0 = svasr_wide_n_s8_z (p0, z1, 7),
		z0 = svasr_wide_z (p0, z1, 7))

/*
** asr_wide_8_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	asr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (asr_wide_8_s8_z_tied1, svint8_t,
		z0 = svasr_wide_n_s8_z (p0, z0, 8),
		z0 = svasr_wide_z (p0, z0, 8))

/*
** asr_wide_8_s8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	asr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (asr_wide_8_s8_z_untied, svint8_t,
		z0 = svasr_wide_n_s8_z (p0, z1, 8),
		z0 = svasr_wide_z (p0, z1, 8))

/*
** asr_wide_s8_x_tied1:
**	asr	z0\.b, z0\.b, z4\.d
**	ret
*/
TEST_DUAL_Z (asr_wide_s8_x_tied1, svint8_t, svuint64_t,
	     z0 = svasr_wide_s8_x (p0, z0, z4),
	     z0 = svasr_wide_x (p0, z0, z4))

/*
** asr_wide_s8_x_tied2:
**	asr	z0\.b, z4\.b, z0\.d
**	ret
*/
TEST_DUAL_Z_REV (asr_wide_s8_x_tied2, svint8_t, svuint64_t,
		 z0_res = svasr_wide_s8_x (p0, z4, z0),
		 z0_res = svasr_wide_x (p0, z4, z0))

/*
** asr_wide_s8_x_untied:
**	asr	z0\.b, z1\.b, z4\.d
**	ret
*/
TEST_DUAL_Z (asr_wide_s8_x_untied, svint8_t, svuint64_t,
	     z0 = svasr_wide_s8_x (p0, z1, z4),
	     z0 = svasr_wide_x (p0, z1, z4))

/*
** asr_wide_x0_s8_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	asr	z0\.b, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (asr_wide_x0_s8_x_tied1, svint8_t, uint64_t,
		 z0 = svasr_wide_n_s8_x (p0, z0, x0),
		 z0 = svasr_wide_x (p0, z0, x0))

/*
** asr_wide_x0_s8_x_untied:
**	mov	(z[0-9]+\.d), x0
**	asr	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (asr_wide_x0_s8_x_untied, svint8_t, uint64_t,
		 z0 = svasr_wide_n_s8_x (p0, z1, x0),
		 z0 = svasr_wide_x (p0, z1, x0))

/*
** asr_wide_1_s8_x_tied1:
**	asr	z0\.b, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (asr_wide_1_s8_x_tied1, svint8_t,
		z0 = svasr_wide_n_s8_x (p0, z0, 1),
		z0 = svasr_wide_x (p0, z0, 1))

/*
** asr_wide_1_s8_x_untied:
**	asr	z0\.b, z1\.b, #1
**	ret
*/
TEST_UNIFORM_Z (asr_wide_1_s8_x_untied, svint8_t,
		z0 = svasr_wide_n_s8_x (p0, z1, 1),
		z0 = svasr_wide_x (p0, z1, 1))

/*
** asr_wide_7_s8_x_tied1:
**	asr	z0\.b, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (asr_wide_7_s8_x_tied1, svint8_t,
		z0 = svasr_wide_n_s8_x (p0, z0, 7),
		z0 = svasr_wide_x (p0, z0, 7))

/*
** asr_wide_7_s8_x_untied:
**	asr	z0\.b, z1\.b, #7
**	ret
*/
TEST_UNIFORM_Z (asr_wide_7_s8_x_untied, svint8_t,
		z0 = svasr_wide_n_s8_x (p0, z1, 7),
		z0 = svasr_wide_x (p0, z1, 7))

/*
** asr_wide_8_s8_x_tied1:
**	asr	z0\.b, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (asr_wide_8_s8_x_tied1, svint8_t,
		z0 = svasr_wide_n_s8_x (p0, z0, 8),
		z0 = svasr_wide_x (p0, z0, 8))

/*
** asr_wide_8_s8_x_untied:
**	asr	z0\.b, z1\.b, #8
**	ret
*/
TEST_UNIFORM_Z (asr_wide_8_s8_x_untied, svint8_t,
		z0 = svasr_wide_n_s8_x (p0, z1, 8),
		z0 = svasr_wide_x (p0, z1, 8))
