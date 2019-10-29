/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** asr_s8_m_tied1:
**	asr	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (asr_s8_m_tied1, svint8_t, svuint8_t,
	     z0 = svasr_s8_m (p0, z0, z4),
	     z0 = svasr_m (p0, z0, z4))

/*
** asr_s8_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	asr	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_DUAL_Z_REV (asr_s8_m_tied2, svint8_t, svuint8_t,
		 z0_res = svasr_s8_m (p0, z4, z0),
		 z0_res = svasr_m (p0, z4, z0))

/*
** asr_s8_m_untied:
**	movprfx	z0, z1
**	asr	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (asr_s8_m_untied, svint8_t, svuint8_t,
	     z0 = svasr_s8_m (p0, z1, z4),
	     z0 = svasr_m (p0, z1, z4))

/*
** asr_w0_s8_m_tied1:
**	mov	(z[0-9]+\.b), w0
**	asr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (asr_w0_s8_m_tied1, svint8_t, uint8_t,
		 z0 = svasr_n_s8_m (p0, z0, x0),
		 z0 = svasr_m (p0, z0, x0))

/*
** asr_w0_s8_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	asr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (asr_w0_s8_m_untied, svint8_t, uint8_t,
		 z0 = svasr_n_s8_m (p0, z1, x0),
		 z0 = svasr_m (p0, z1, x0))

/*
** asr_1_s8_m_tied1:
**	asr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (asr_1_s8_m_tied1, svint8_t,
		z0 = svasr_n_s8_m (p0, z0, 1),
		z0 = svasr_m (p0, z0, 1))

/*
** asr_1_s8_m_untied:
**	movprfx	z0, z1
**	asr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (asr_1_s8_m_untied, svint8_t,
		z0 = svasr_n_s8_m (p0, z1, 1),
		z0 = svasr_m (p0, z1, 1))

/*
** asr_7_s8_m_tied1:
**	asr	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (asr_7_s8_m_tied1, svint8_t,
		z0 = svasr_n_s8_m (p0, z0, 7),
		z0 = svasr_m (p0, z0, 7))

/*
** asr_7_s8_m_untied:
**	movprfx	z0, z1
**	asr	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (asr_7_s8_m_untied, svint8_t,
		z0 = svasr_n_s8_m (p0, z1, 7),
		z0 = svasr_m (p0, z1, 7))

/*
** asr_8_s8_m_tied1:
**	asr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (asr_8_s8_m_tied1, svint8_t,
		z0 = svasr_n_s8_m (p0, z0, 8),
		z0 = svasr_m (p0, z0, 8))

/*
** asr_8_s8_m_untied:
**	movprfx	z0, z1
**	asr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (asr_8_s8_m_untied, svint8_t,
		z0 = svasr_n_s8_m (p0, z1, 8),
		z0 = svasr_m (p0, z1, 8))

/*
** asr_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	asr	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (asr_s8_z_tied1, svint8_t, svuint8_t,
	     z0 = svasr_s8_z (p0, z0, z4),
	     z0 = svasr_z (p0, z0, z4))

/*
** asr_s8_z_tied2:
**	movprfx	z0\.b, p0/z, z0\.b
**	asrr	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z_REV (asr_s8_z_tied2, svint8_t, svuint8_t,
		 z0_res = svasr_s8_z (p0, z4, z0),
		 z0_res = svasr_z (p0, z4, z0))

/*
** asr_s8_z_untied:
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	asr	z0\.b, p0/m, z0\.b, z4\.b
** |
**	movprfx	z0\.b, p0/z, z4\.b
**	asrr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_DUAL_Z (asr_s8_z_untied, svint8_t, svuint8_t,
	     z0 = svasr_s8_z (p0, z1, z4),
	     z0 = svasr_z (p0, z1, z4))

/*
** asr_w0_s8_z_tied1:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0\.b, p0/z, z0\.b
**	asr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (asr_w0_s8_z_tied1, svint8_t, uint8_t,
		 z0 = svasr_n_s8_z (p0, z0, x0),
		 z0 = svasr_z (p0, z0, x0))

/*
** asr_w0_s8_z_untied:
**	mov	(z[0-9]+\.b), w0
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	asr	z0\.b, p0/m, z0\.b, \1
** |
**	movprfx	z0\.b, p0/z, \1
**	asrr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_ZX (asr_w0_s8_z_untied, svint8_t, uint8_t,
		 z0 = svasr_n_s8_z (p0, z1, x0),
		 z0 = svasr_z (p0, z1, x0))

/*
** asr_1_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	asr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (asr_1_s8_z_tied1, svint8_t,
		z0 = svasr_n_s8_z (p0, z0, 1),
		z0 = svasr_z (p0, z0, 1))

/*
** asr_1_s8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	asr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (asr_1_s8_z_untied, svint8_t,
		z0 = svasr_n_s8_z (p0, z1, 1),
		z0 = svasr_z (p0, z1, 1))

/*
** asr_7_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	asr	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (asr_7_s8_z_tied1, svint8_t,
		z0 = svasr_n_s8_z (p0, z0, 7),
		z0 = svasr_z (p0, z0, 7))

/*
** asr_7_s8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	asr	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (asr_7_s8_z_untied, svint8_t,
		z0 = svasr_n_s8_z (p0, z1, 7),
		z0 = svasr_z (p0, z1, 7))

/*
** asr_8_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	asr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (asr_8_s8_z_tied1, svint8_t,
		z0 = svasr_n_s8_z (p0, z0, 8),
		z0 = svasr_z (p0, z0, 8))

/*
** asr_8_s8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	asr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (asr_8_s8_z_untied, svint8_t,
		z0 = svasr_n_s8_z (p0, z1, 8),
		z0 = svasr_z (p0, z1, 8))

/*
** asr_s8_x_tied1:
**	asr	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (asr_s8_x_tied1, svint8_t, svuint8_t,
	     z0 = svasr_s8_x (p0, z0, z4),
	     z0 = svasr_x (p0, z0, z4))

/*
** asr_s8_x_tied2:
**	asrr	z0\.b, p0/m, z0\.b, z4\.b
**	ret
*/
TEST_DUAL_Z_REV (asr_s8_x_tied2, svint8_t, svuint8_t,
		 z0_res = svasr_s8_x (p0, z4, z0),
		 z0_res = svasr_x (p0, z4, z0))

/*
** asr_s8_x_untied:
** (
**	movprfx	z0, z1
**	asr	z0\.b, p0/m, z0\.b, z4\.b
** |
**	movprfx	z0, z4
**	asrr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_DUAL_Z (asr_s8_x_untied, svint8_t, svuint8_t,
	     z0 = svasr_s8_x (p0, z1, z4),
	     z0 = svasr_x (p0, z1, z4))

/*
** asr_w0_s8_x_tied1:
**	mov	(z[0-9]+\.b), w0
**	asr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (asr_w0_s8_x_tied1, svint8_t, uint8_t,
		 z0 = svasr_n_s8_x (p0, z0, x0),
		 z0 = svasr_x (p0, z0, x0))

/*
** asr_w0_s8_x_untied:
**	mov	z0\.b, w0
**	asrr	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_ZX (asr_w0_s8_x_untied, svint8_t, uint8_t,
		 z0 = svasr_n_s8_x (p0, z1, x0),
		 z0 = svasr_x (p0, z1, x0))

/*
** asr_1_s8_x_tied1:
**	asr	z0\.b, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (asr_1_s8_x_tied1, svint8_t,
		z0 = svasr_n_s8_x (p0, z0, 1),
		z0 = svasr_x (p0, z0, 1))

/*
** asr_1_s8_x_untied:
**	asr	z0\.b, z1\.b, #1
**	ret
*/
TEST_UNIFORM_Z (asr_1_s8_x_untied, svint8_t,
		z0 = svasr_n_s8_x (p0, z1, 1),
		z0 = svasr_x (p0, z1, 1))

/*
** asr_7_s8_x_tied1:
**	asr	z0\.b, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (asr_7_s8_x_tied1, svint8_t,
		z0 = svasr_n_s8_x (p0, z0, 7),
		z0 = svasr_x (p0, z0, 7))

/*
** asr_7_s8_x_untied:
**	asr	z0\.b, z1\.b, #7
**	ret
*/
TEST_UNIFORM_Z (asr_7_s8_x_untied, svint8_t,
		z0 = svasr_n_s8_x (p0, z1, 7),
		z0 = svasr_x (p0, z1, 7))

/*
** asr_8_s8_x_tied1:
**	asr	z0\.b, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (asr_8_s8_x_tied1, svint8_t,
		z0 = svasr_n_s8_x (p0, z0, 8),
		z0 = svasr_x (p0, z0, 8))

/*
** asr_8_s8_x_untied:
**	asr	z0\.b, z1\.b, #8
**	ret
*/
TEST_UNIFORM_Z (asr_8_s8_x_untied, svint8_t,
		z0 = svasr_n_s8_x (p0, z1, 8),
		z0 = svasr_x (p0, z1, 8))
