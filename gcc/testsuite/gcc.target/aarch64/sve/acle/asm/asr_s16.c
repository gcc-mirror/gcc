/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** asr_s16_m_tied1:
**	asr	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (asr_s16_m_tied1, svint16_t, svuint16_t,
	     z0 = svasr_s16_m (p0, z0, z4),
	     z0 = svasr_m (p0, z0, z4))

/*
** asr_s16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	asr	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (asr_s16_m_tied2, svint16_t, svuint16_t,
		 z0_res = svasr_s16_m (p0, z4, z0),
		 z0_res = svasr_m (p0, z4, z0))

/*
** asr_s16_m_untied:
**	movprfx	z0, z1
**	asr	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (asr_s16_m_untied, svint16_t, svuint16_t,
	     z0 = svasr_s16_m (p0, z1, z4),
	     z0 = svasr_m (p0, z1, z4))

/*
** asr_w0_s16_m_tied1:
**	mov	(z[0-9]+\.h), w0
**	asr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (asr_w0_s16_m_tied1, svint16_t, uint16_t,
		 z0 = svasr_n_s16_m (p0, z0, x0),
		 z0 = svasr_m (p0, z0, x0))

/*
** asr_w0_s16_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	asr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (asr_w0_s16_m_untied, svint16_t, uint16_t,
		 z0 = svasr_n_s16_m (p0, z1, x0),
		 z0 = svasr_m (p0, z1, x0))

/*
** asr_1_s16_m_tied1:
**	asr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (asr_1_s16_m_tied1, svint16_t,
		z0 = svasr_n_s16_m (p0, z0, 1),
		z0 = svasr_m (p0, z0, 1))

/*
** asr_1_s16_m_untied:
**	movprfx	z0, z1
**	asr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (asr_1_s16_m_untied, svint16_t,
		z0 = svasr_n_s16_m (p0, z1, 1),
		z0 = svasr_m (p0, z1, 1))

/*
** asr_15_s16_m_tied1:
**	asr	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (asr_15_s16_m_tied1, svint16_t,
		z0 = svasr_n_s16_m (p0, z0, 15),
		z0 = svasr_m (p0, z0, 15))

/*
** asr_15_s16_m_untied:
**	movprfx	z0, z1
**	asr	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (asr_15_s16_m_untied, svint16_t,
		z0 = svasr_n_s16_m (p0, z1, 15),
		z0 = svasr_m (p0, z1, 15))

/*
** asr_16_s16_m_tied1:
**	asr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (asr_16_s16_m_tied1, svint16_t,
		z0 = svasr_n_s16_m (p0, z0, 16),
		z0 = svasr_m (p0, z0, 16))

/*
** asr_16_s16_m_untied:
**	movprfx	z0, z1
**	asr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (asr_16_s16_m_untied, svint16_t,
		z0 = svasr_n_s16_m (p0, z1, 16),
		z0 = svasr_m (p0, z1, 16))

/*
** asr_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	asr	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (asr_s16_z_tied1, svint16_t, svuint16_t,
	     z0 = svasr_s16_z (p0, z0, z4),
	     z0 = svasr_z (p0, z0, z4))

/*
** asr_s16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	asrr	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z_REV (asr_s16_z_tied2, svint16_t, svuint16_t,
		 z0_res = svasr_s16_z (p0, z4, z0),
		 z0_res = svasr_z (p0, z4, z0))

/*
** asr_s16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	asr	z0\.h, p0/m, z0\.h, z4\.h
** |
**	movprfx	z0\.h, p0/z, z4\.h
**	asrr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_DUAL_Z (asr_s16_z_untied, svint16_t, svuint16_t,
	     z0 = svasr_s16_z (p0, z1, z4),
	     z0 = svasr_z (p0, z1, z4))

/*
** asr_w0_s16_z_tied1:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	asr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (asr_w0_s16_z_tied1, svint16_t, uint16_t,
		 z0 = svasr_n_s16_z (p0, z0, x0),
		 z0 = svasr_z (p0, z0, x0))

/*
** asr_w0_s16_z_untied:
**	mov	(z[0-9]+\.h), w0
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	asr	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	asrr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZX (asr_w0_s16_z_untied, svint16_t, uint16_t,
		 z0 = svasr_n_s16_z (p0, z1, x0),
		 z0 = svasr_z (p0, z1, x0))

/*
** asr_1_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	asr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (asr_1_s16_z_tied1, svint16_t,
		z0 = svasr_n_s16_z (p0, z0, 1),
		z0 = svasr_z (p0, z0, 1))

/*
** asr_1_s16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	asr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (asr_1_s16_z_untied, svint16_t,
		z0 = svasr_n_s16_z (p0, z1, 1),
		z0 = svasr_z (p0, z1, 1))

/*
** asr_15_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	asr	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (asr_15_s16_z_tied1, svint16_t,
		z0 = svasr_n_s16_z (p0, z0, 15),
		z0 = svasr_z (p0, z0, 15))

/*
** asr_15_s16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	asr	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (asr_15_s16_z_untied, svint16_t,
		z0 = svasr_n_s16_z (p0, z1, 15),
		z0 = svasr_z (p0, z1, 15))

/*
** asr_16_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	asr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (asr_16_s16_z_tied1, svint16_t,
		z0 = svasr_n_s16_z (p0, z0, 16),
		z0 = svasr_z (p0, z0, 16))

/*
** asr_16_s16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	asr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (asr_16_s16_z_untied, svint16_t,
		z0 = svasr_n_s16_z (p0, z1, 16),
		z0 = svasr_z (p0, z1, 16))

/*
** asr_s16_x_tied1:
**	asr	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z (asr_s16_x_tied1, svint16_t, svuint16_t,
	     z0 = svasr_s16_x (p0, z0, z4),
	     z0 = svasr_x (p0, z0, z4))

/*
** asr_s16_x_tied2:
**	asrr	z0\.h, p0/m, z0\.h, z4\.h
**	ret
*/
TEST_DUAL_Z_REV (asr_s16_x_tied2, svint16_t, svuint16_t,
		 z0_res = svasr_s16_x (p0, z4, z0),
		 z0_res = svasr_x (p0, z4, z0))

/*
** asr_s16_x_untied:
** (
**	movprfx	z0, z1
**	asr	z0\.h, p0/m, z0\.h, z4\.h
** |
**	movprfx	z0, z4
**	asrr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_DUAL_Z (asr_s16_x_untied, svint16_t, svuint16_t,
	     z0 = svasr_s16_x (p0, z1, z4),
	     z0 = svasr_x (p0, z1, z4))

/*
** asr_w0_s16_x_tied1:
**	mov	(z[0-9]+\.h), w0
**	asr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (asr_w0_s16_x_tied1, svint16_t, uint16_t,
		 z0 = svasr_n_s16_x (p0, z0, x0),
		 z0 = svasr_x (p0, z0, x0))

/*
** asr_w0_s16_x_untied:
**	mov	z0\.h, w0
**	asrr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZX (asr_w0_s16_x_untied, svint16_t, uint16_t,
		 z0 = svasr_n_s16_x (p0, z1, x0),
		 z0 = svasr_x (p0, z1, x0))

/*
** asr_1_s16_x_tied1:
**	asr	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (asr_1_s16_x_tied1, svint16_t,
		z0 = svasr_n_s16_x (p0, z0, 1),
		z0 = svasr_x (p0, z0, 1))

/*
** asr_1_s16_x_untied:
**	asr	z0\.h, z1\.h, #1
**	ret
*/
TEST_UNIFORM_Z (asr_1_s16_x_untied, svint16_t,
		z0 = svasr_n_s16_x (p0, z1, 1),
		z0 = svasr_x (p0, z1, 1))

/*
** asr_15_s16_x_tied1:
**	asr	z0\.h, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (asr_15_s16_x_tied1, svint16_t,
		z0 = svasr_n_s16_x (p0, z0, 15),
		z0 = svasr_x (p0, z0, 15))

/*
** asr_15_s16_x_untied:
**	asr	z0\.h, z1\.h, #15
**	ret
*/
TEST_UNIFORM_Z (asr_15_s16_x_untied, svint16_t,
		z0 = svasr_n_s16_x (p0, z1, 15),
		z0 = svasr_x (p0, z1, 15))

/*
** asr_16_s16_x_tied1:
**	asr	z0\.h, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (asr_16_s16_x_tied1, svint16_t,
		z0 = svasr_n_s16_x (p0, z0, 16),
		z0 = svasr_x (p0, z0, 16))

/*
** asr_16_s16_x_untied:
**	asr	z0\.h, z1\.h, #16
**	ret
*/
TEST_UNIFORM_Z (asr_16_s16_x_untied, svint16_t,
		z0 = svasr_n_s16_x (p0, z1, 16),
		z0 = svasr_x (p0, z1, 16))
