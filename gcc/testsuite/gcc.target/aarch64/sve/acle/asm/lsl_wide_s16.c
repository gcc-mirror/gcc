/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lsl_wide_s16_m_tied1:
**	lsl	z0\.h, p0/m, z0\.h, z4\.d
**	ret
*/
TEST_DUAL_Z (lsl_wide_s16_m_tied1, svint16_t, svuint64_t,
	     z0 = svlsl_wide_s16_m (p0, z0, z4),
	     z0 = svlsl_wide_m (p0, z0, z4))

/*
** lsl_wide_s16_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z4
**	lsl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_DUAL_Z_REV (lsl_wide_s16_m_tied2, svint16_t, svuint64_t,
		 z0_res = svlsl_wide_s16_m (p0, z4, z0),
		 z0_res = svlsl_wide_m (p0, z4, z0))

/*
** lsl_wide_s16_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.h, p0/m, z0\.h, z4\.d
**	ret
*/
TEST_DUAL_Z (lsl_wide_s16_m_untied, svint16_t, svuint64_t,
	     z0 = svlsl_wide_s16_m (p0, z1, z4),
	     z0 = svlsl_wide_m (p0, z1, z4))

/*
** lsl_wide_x0_s16_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	lsl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_wide_x0_s16_m_tied1, svint16_t, uint64_t,
		 z0 = svlsl_wide_n_s16_m (p0, z0, x0),
		 z0 = svlsl_wide_m (p0, z0, x0))

/*
** lsl_wide_x0_s16_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	lsl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_wide_x0_s16_m_untied, svint16_t, uint64_t,
		 z0 = svlsl_wide_n_s16_m (p0, z1, x0),
		 z0 = svlsl_wide_m (p0, z1, x0))

/*
** lsl_wide_1_s16_m_tied1:
**	lsl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_1_s16_m_tied1, svint16_t,
		z0 = svlsl_wide_n_s16_m (p0, z0, 1),
		z0 = svlsl_wide_m (p0, z0, 1))

/*
** lsl_wide_1_s16_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_1_s16_m_untied, svint16_t,
		z0 = svlsl_wide_n_s16_m (p0, z1, 1),
		z0 = svlsl_wide_m (p0, z1, 1))

/*
** lsl_wide_15_s16_m_tied1:
**	lsl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_15_s16_m_tied1, svint16_t,
		z0 = svlsl_wide_n_s16_m (p0, z0, 15),
		z0 = svlsl_wide_m (p0, z0, 15))

/*
** lsl_wide_15_s16_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_15_s16_m_untied, svint16_t,
		z0 = svlsl_wide_n_s16_m (p0, z1, 15),
		z0 = svlsl_wide_m (p0, z1, 15))

/*
** lsl_wide_16_s16_m_tied1:
**	mov	(z[0-9]+\.d), #16
**	lsl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_16_s16_m_tied1, svint16_t,
		z0 = svlsl_wide_n_s16_m (p0, z0, 16),
		z0 = svlsl_wide_m (p0, z0, 16))

/*
** lsl_wide_16_s16_m_untied:
**	mov	(z[0-9]+\.d), #16
**	movprfx	z0, z1
**	lsl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_16_s16_m_untied, svint16_t,
		z0 = svlsl_wide_n_s16_m (p0, z1, 16),
		z0 = svlsl_wide_m (p0, z1, 16))

/*
** lsl_wide_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsl	z0\.h, p0/m, z0\.h, z4\.d
**	ret
*/
TEST_DUAL_Z (lsl_wide_s16_z_tied1, svint16_t, svuint64_t,
	     z0 = svlsl_wide_s16_z (p0, z0, z4),
	     z0 = svlsl_wide_z (p0, z0, z4))

/*
** lsl_wide_s16_z_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.h, p0/z, z4\.h
**	lsl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_DUAL_Z_REV (lsl_wide_s16_z_tied2, svint16_t, svuint64_t,
		 z0_res = svlsl_wide_s16_z (p0, z4, z0),
		 z0_res = svlsl_wide_z (p0, z4, z0))

/*
** lsl_wide_s16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	lsl	z0\.h, p0/m, z0\.h, z4\.d
**	ret
*/
TEST_DUAL_Z (lsl_wide_s16_z_untied, svint16_t, svuint64_t,
	     z0 = svlsl_wide_s16_z (p0, z1, z4),
	     z0 = svlsl_wide_z (p0, z1, z4))

/*
** lsl_wide_x0_s16_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.h, p0/z, z0\.h
**	lsl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_wide_x0_s16_z_tied1, svint16_t, uint64_t,
		 z0 = svlsl_wide_n_s16_z (p0, z0, x0),
		 z0 = svlsl_wide_z (p0, z0, x0))

/*
** lsl_wide_x0_s16_z_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.h, p0/z, z1\.h
**	lsl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_wide_x0_s16_z_untied, svint16_t, uint64_t,
		 z0 = svlsl_wide_n_s16_z (p0, z1, x0),
		 z0 = svlsl_wide_z (p0, z1, x0))

/*
** lsl_wide_1_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_1_s16_z_tied1, svint16_t,
		z0 = svlsl_wide_n_s16_z (p0, z0, 1),
		z0 = svlsl_wide_z (p0, z0, 1))

/*
** lsl_wide_1_s16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	lsl	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_1_s16_z_untied, svint16_t,
		z0 = svlsl_wide_n_s16_z (p0, z1, 1),
		z0 = svlsl_wide_z (p0, z1, 1))

/*
** lsl_wide_15_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_15_s16_z_tied1, svint16_t,
		z0 = svlsl_wide_n_s16_z (p0, z0, 15),
		z0 = svlsl_wide_z (p0, z0, 15))

/*
** lsl_wide_15_s16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	lsl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_15_s16_z_untied, svint16_t,
		z0 = svlsl_wide_n_s16_z (p0, z1, 15),
		z0 = svlsl_wide_z (p0, z1, 15))

/*
** lsl_wide_16_s16_z_tied1:
**	mov	(z[0-9]+\.d), #16
**	movprfx	z0\.h, p0/z, z0\.h
**	lsl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_16_s16_z_tied1, svint16_t,
		z0 = svlsl_wide_n_s16_z (p0, z0, 16),
		z0 = svlsl_wide_z (p0, z0, 16))

/*
** lsl_wide_16_s16_z_untied:
**	mov	(z[0-9]+\.d), #16
**	movprfx	z0\.h, p0/z, z1\.h
**	lsl	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_16_s16_z_untied, svint16_t,
		z0 = svlsl_wide_n_s16_z (p0, z1, 16),
		z0 = svlsl_wide_z (p0, z1, 16))

/*
** lsl_wide_s16_x_tied1:
**	lsl	z0\.h, z0\.h, z4\.d
**	ret
*/
TEST_DUAL_Z (lsl_wide_s16_x_tied1, svint16_t, svuint64_t,
	     z0 = svlsl_wide_s16_x (p0, z0, z4),
	     z0 = svlsl_wide_x (p0, z0, z4))

/*
** lsl_wide_s16_x_tied2:
**	lsl	z0\.h, z4\.h, z0\.d
**	ret
*/
TEST_DUAL_Z_REV (lsl_wide_s16_x_tied2, svint16_t, svuint64_t,
		 z0_res = svlsl_wide_s16_x (p0, z4, z0),
		 z0_res = svlsl_wide_x (p0, z4, z0))

/*
** lsl_wide_s16_x_untied:
**	lsl	z0\.h, z1\.h, z4\.d
**	ret
*/
TEST_DUAL_Z (lsl_wide_s16_x_untied, svint16_t, svuint64_t,
	     z0 = svlsl_wide_s16_x (p0, z1, z4),
	     z0 = svlsl_wide_x (p0, z1, z4))

/*
** lsl_wide_x0_s16_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	lsl	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_wide_x0_s16_x_tied1, svint16_t, uint64_t,
		 z0 = svlsl_wide_n_s16_x (p0, z0, x0),
		 z0 = svlsl_wide_x (p0, z0, x0))

/*
** lsl_wide_x0_s16_x_untied:
**	mov	(z[0-9]+\.d), x0
**	lsl	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_wide_x0_s16_x_untied, svint16_t, uint64_t,
		 z0 = svlsl_wide_n_s16_x (p0, z1, x0),
		 z0 = svlsl_wide_x (p0, z1, x0))

/*
** lsl_wide_1_s16_x_tied1:
**	lsl	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_1_s16_x_tied1, svint16_t,
		z0 = svlsl_wide_n_s16_x (p0, z0, 1),
		z0 = svlsl_wide_x (p0, z0, 1))

/*
** lsl_wide_1_s16_x_untied:
**	lsl	z0\.h, z1\.h, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_1_s16_x_untied, svint16_t,
		z0 = svlsl_wide_n_s16_x (p0, z1, 1),
		z0 = svlsl_wide_x (p0, z1, 1))

/*
** lsl_wide_15_s16_x_tied1:
**	lsl	z0\.h, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_15_s16_x_tied1, svint16_t,
		z0 = svlsl_wide_n_s16_x (p0, z0, 15),
		z0 = svlsl_wide_x (p0, z0, 15))

/*
** lsl_wide_15_s16_x_untied:
**	lsl	z0\.h, z1\.h, #15
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_15_s16_x_untied, svint16_t,
		z0 = svlsl_wide_n_s16_x (p0, z1, 15),
		z0 = svlsl_wide_x (p0, z1, 15))

/*
** lsl_wide_16_s16_x_tied1:
**	mov	(z[0-9]+\.d), #16
**	lsl	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_16_s16_x_tied1, svint16_t,
		z0 = svlsl_wide_n_s16_x (p0, z0, 16),
		z0 = svlsl_wide_x (p0, z0, 16))

/*
** lsl_wide_16_s16_x_untied:
**	mov	(z[0-9]+\.d), #16
**	lsl	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_16_s16_x_untied, svint16_t,
		z0 = svlsl_wide_n_s16_x (p0, z1, 16),
		z0 = svlsl_wide_x (p0, z1, 16))
