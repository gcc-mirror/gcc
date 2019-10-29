/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** asr_s64_m_tied1:
**	asr	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (asr_s64_m_tied1, svint64_t, svuint64_t,
	     z0 = svasr_s64_m (p0, z0, z4),
	     z0 = svasr_m (p0, z0, z4))

/*
** asr_s64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z4
**	asr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_DUAL_Z_REV (asr_s64_m_tied2, svint64_t, svuint64_t,
		 z0_res = svasr_s64_m (p0, z4, z0),
		 z0_res = svasr_m (p0, z4, z0))

/*
** asr_s64_m_untied:
**	movprfx	z0, z1
**	asr	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (asr_s64_m_untied, svint64_t, svuint64_t,
	     z0 = svasr_s64_m (p0, z1, z4),
	     z0 = svasr_m (p0, z1, z4))

/*
** asr_x0_s64_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	asr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (asr_x0_s64_m_tied1, svint64_t, uint64_t,
		 z0 = svasr_n_s64_m (p0, z0, x0),
		 z0 = svasr_m (p0, z0, x0))

/*
** asr_x0_s64_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	asr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (asr_x0_s64_m_untied, svint64_t, uint64_t,
		 z0 = svasr_n_s64_m (p0, z1, x0),
		 z0 = svasr_m (p0, z1, x0))

/*
** asr_1_s64_m_tied1:
**	asr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (asr_1_s64_m_tied1, svint64_t,
		z0 = svasr_n_s64_m (p0, z0, 1),
		z0 = svasr_m (p0, z0, 1))

/*
** asr_1_s64_m_untied:
**	movprfx	z0, z1
**	asr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (asr_1_s64_m_untied, svint64_t,
		z0 = svasr_n_s64_m (p0, z1, 1),
		z0 = svasr_m (p0, z1, 1))

/*
** asr_63_s64_m_tied1:
**	asr	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (asr_63_s64_m_tied1, svint64_t,
		z0 = svasr_n_s64_m (p0, z0, 63),
		z0 = svasr_m (p0, z0, 63))

/*
** asr_63_s64_m_untied:
**	movprfx	z0, z1
**	asr	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (asr_63_s64_m_untied, svint64_t,
		z0 = svasr_n_s64_m (p0, z1, 63),
		z0 = svasr_m (p0, z1, 63))

/*
** asr_64_s64_m_tied1:
**	asr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (asr_64_s64_m_tied1, svint64_t,
		z0 = svasr_n_s64_m (p0, z0, 64),
		z0 = svasr_m (p0, z0, 64))

/*
** asr_64_s64_m_untied:
**	movprfx	z0, z1
**	asr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (asr_64_s64_m_untied, svint64_t,
		z0 = svasr_n_s64_m (p0, z1, 64),
		z0 = svasr_m (p0, z1, 64))

/*
** asr_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	asr	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (asr_s64_z_tied1, svint64_t, svuint64_t,
	     z0 = svasr_s64_z (p0, z0, z4),
	     z0 = svasr_z (p0, z0, z4))

/*
** asr_s64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	asrr	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z_REV (asr_s64_z_tied2, svint64_t, svuint64_t,
		 z0_res = svasr_s64_z (p0, z4, z0),
		 z0_res = svasr_z (p0, z4, z0))

/*
** asr_s64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	asr	z0\.d, p0/m, z0\.d, z4\.d
** |
**	movprfx	z0\.d, p0/z, z4\.d
**	asrr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (asr_s64_z_untied, svint64_t, svuint64_t,
	     z0 = svasr_s64_z (p0, z1, z4),
	     z0 = svasr_z (p0, z1, z4))

/*
** asr_x0_s64_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	asr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (asr_x0_s64_z_tied1, svint64_t, uint64_t,
		 z0 = svasr_n_s64_z (p0, z0, x0),
		 z0 = svasr_z (p0, z0, x0))

/*
** asr_x0_s64_z_untied:
**	mov	(z[0-9]+\.d), x0
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	asr	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	asrr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZX (asr_x0_s64_z_untied, svint64_t, uint64_t,
		 z0 = svasr_n_s64_z (p0, z1, x0),
		 z0 = svasr_z (p0, z1, x0))

/*
** asr_1_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	asr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (asr_1_s64_z_tied1, svint64_t,
		z0 = svasr_n_s64_z (p0, z0, 1),
		z0 = svasr_z (p0, z0, 1))

/*
** asr_1_s64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	asr	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (asr_1_s64_z_untied, svint64_t,
		z0 = svasr_n_s64_z (p0, z1, 1),
		z0 = svasr_z (p0, z1, 1))

/*
** asr_63_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	asr	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (asr_63_s64_z_tied1, svint64_t,
		z0 = svasr_n_s64_z (p0, z0, 63),
		z0 = svasr_z (p0, z0, 63))

/*
** asr_63_s64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	asr	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (asr_63_s64_z_untied, svint64_t,
		z0 = svasr_n_s64_z (p0, z1, 63),
		z0 = svasr_z (p0, z1, 63))

/*
** asr_64_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	asr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (asr_64_s64_z_tied1, svint64_t,
		z0 = svasr_n_s64_z (p0, z0, 64),
		z0 = svasr_z (p0, z0, 64))

/*
** asr_64_s64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	asr	z0\.d, p0/m, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (asr_64_s64_z_untied, svint64_t,
		z0 = svasr_n_s64_z (p0, z1, 64),
		z0 = svasr_z (p0, z1, 64))

/*
** asr_s64_x_tied1:
**	asr	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z (asr_s64_x_tied1, svint64_t, svuint64_t,
	     z0 = svasr_s64_x (p0, z0, z4),
	     z0 = svasr_x (p0, z0, z4))

/*
** asr_s64_x_tied2:
**	asrr	z0\.d, p0/m, z0\.d, z4\.d
**	ret
*/
TEST_DUAL_Z_REV (asr_s64_x_tied2, svint64_t, svuint64_t,
		 z0_res = svasr_s64_x (p0, z4, z0),
		 z0_res = svasr_x (p0, z4, z0))

/*
** asr_s64_x_untied:
** (
**	movprfx	z0, z1
**	asr	z0\.d, p0/m, z0\.d, z4\.d
** |
**	movprfx	z0, z4
**	asrr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (asr_s64_x_untied, svint64_t, svuint64_t,
	     z0 = svasr_s64_x (p0, z1, z4),
	     z0 = svasr_x (p0, z1, z4))

/*
** asr_x0_s64_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	asr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (asr_x0_s64_x_tied1, svint64_t, uint64_t,
		 z0 = svasr_n_s64_x (p0, z0, x0),
		 z0 = svasr_x (p0, z0, x0))

/*
** asr_x0_s64_x_untied:
**	mov	z0\.d, x0
**	asrr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_ZX (asr_x0_s64_x_untied, svint64_t, uint64_t,
		 z0 = svasr_n_s64_x (p0, z1, x0),
		 z0 = svasr_x (p0, z1, x0))

/*
** asr_1_s64_x_tied1:
**	asr	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (asr_1_s64_x_tied1, svint64_t,
		z0 = svasr_n_s64_x (p0, z0, 1),
		z0 = svasr_x (p0, z0, 1))

/*
** asr_1_s64_x_untied:
**	asr	z0\.d, z1\.d, #1
**	ret
*/
TEST_UNIFORM_Z (asr_1_s64_x_untied, svint64_t,
		z0 = svasr_n_s64_x (p0, z1, 1),
		z0 = svasr_x (p0, z1, 1))

/*
** asr_63_s64_x_tied1:
**	asr	z0\.d, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (asr_63_s64_x_tied1, svint64_t,
		z0 = svasr_n_s64_x (p0, z0, 63),
		z0 = svasr_x (p0, z0, 63))

/*
** asr_63_s64_x_untied:
**	asr	z0\.d, z1\.d, #63
**	ret
*/
TEST_UNIFORM_Z (asr_63_s64_x_untied, svint64_t,
		z0 = svasr_n_s64_x (p0, z1, 63),
		z0 = svasr_x (p0, z1, 63))

/*
** asr_64_s64_x_tied1:
**	asr	z0\.d, z0\.d, #64
**	ret
*/
TEST_UNIFORM_Z (asr_64_s64_x_tied1, svint64_t,
		z0 = svasr_n_s64_x (p0, z0, 64),
		z0 = svasr_x (p0, z0, 64))

/*
** asr_64_s64_x_untied:
**	asr	z0\.d, z1\.d, #64
**	ret
*/
TEST_UNIFORM_Z (asr_64_s64_x_untied, svint64_t,
		z0 = svasr_n_s64_x (p0, z1, 64),
		z0 = svasr_x (p0, z1, 64))
