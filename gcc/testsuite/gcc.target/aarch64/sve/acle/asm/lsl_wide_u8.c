/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lsl_wide_u8_m_tied1:
**	lsl	z0\.b, p0/m, z0\.b, z4\.d
**	ret
*/
TEST_DUAL_Z (lsl_wide_u8_m_tied1, svuint8_t, svuint64_t,
	     z0 = svlsl_wide_u8_m (p0, z0, z4),
	     z0 = svlsl_wide_m (p0, z0, z4))

/*
** lsl_wide_u8_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z4
**	lsl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_DUAL_Z_REV (lsl_wide_u8_m_tied2, svuint8_t, svuint64_t,
		 z0_res = svlsl_wide_u8_m (p0, z4, z0),
		 z0_res = svlsl_wide_m (p0, z4, z0))

/*
** lsl_wide_u8_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.b, p0/m, z0\.b, z4\.d
**	ret
*/
TEST_DUAL_Z (lsl_wide_u8_m_untied, svuint8_t, svuint64_t,
	     z0 = svlsl_wide_u8_m (p0, z1, z4),
	     z0 = svlsl_wide_m (p0, z1, z4))

/*
** lsl_wide_x0_u8_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	lsl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_wide_x0_u8_m_tied1, svuint8_t, uint64_t,
		 z0 = svlsl_wide_n_u8_m (p0, z0, x0),
		 z0 = svlsl_wide_m (p0, z0, x0))

/*
** lsl_wide_x0_u8_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	lsl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_wide_x0_u8_m_untied, svuint8_t, uint64_t,
		 z0 = svlsl_wide_n_u8_m (p0, z1, x0),
		 z0 = svlsl_wide_m (p0, z1, x0))

/*
** lsl_wide_1_u8_m_tied1:
**	lsl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_1_u8_m_tied1, svuint8_t,
		z0 = svlsl_wide_n_u8_m (p0, z0, 1),
		z0 = svlsl_wide_m (p0, z0, 1))

/*
** lsl_wide_1_u8_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_1_u8_m_untied, svuint8_t,
		z0 = svlsl_wide_n_u8_m (p0, z1, 1),
		z0 = svlsl_wide_m (p0, z1, 1))

/*
** lsl_wide_7_u8_m_tied1:
**	lsl	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_7_u8_m_tied1, svuint8_t,
		z0 = svlsl_wide_n_u8_m (p0, z0, 7),
		z0 = svlsl_wide_m (p0, z0, 7))

/*
** lsl_wide_7_u8_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_7_u8_m_untied, svuint8_t,
		z0 = svlsl_wide_n_u8_m (p0, z1, 7),
		z0 = svlsl_wide_m (p0, z1, 7))

/*
** lsl_wide_8_u8_m_tied1:
**	mov	(z[0-9]+\.d), #8
**	lsl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_8_u8_m_tied1, svuint8_t,
		z0 = svlsl_wide_n_u8_m (p0, z0, 8),
		z0 = svlsl_wide_m (p0, z0, 8))

/*
** lsl_wide_8_u8_m_untied:
**	mov	(z[0-9]+\.d), #8
**	movprfx	z0, z1
**	lsl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_8_u8_m_untied, svuint8_t,
		z0 = svlsl_wide_n_u8_m (p0, z1, 8),
		z0 = svlsl_wide_m (p0, z1, 8))

/*
** lsl_wide_u8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	lsl	z0\.b, p0/m, z0\.b, z4\.d
**	ret
*/
TEST_DUAL_Z (lsl_wide_u8_z_tied1, svuint8_t, svuint64_t,
	     z0 = svlsl_wide_u8_z (p0, z0, z4),
	     z0 = svlsl_wide_z (p0, z0, z4))

/*
** lsl_wide_u8_z_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.b, p0/z, z4\.b
**	lsl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_DUAL_Z_REV (lsl_wide_u8_z_tied2, svuint8_t, svuint64_t,
		 z0_res = svlsl_wide_u8_z (p0, z4, z0),
		 z0_res = svlsl_wide_z (p0, z4, z0))

/*
** lsl_wide_u8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	lsl	z0\.b, p0/m, z0\.b, z4\.d
**	ret
*/
TEST_DUAL_Z (lsl_wide_u8_z_untied, svuint8_t, svuint64_t,
	     z0 = svlsl_wide_u8_z (p0, z1, z4),
	     z0 = svlsl_wide_z (p0, z1, z4))

/*
** lsl_wide_x0_u8_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.b, p0/z, z0\.b
**	lsl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_wide_x0_u8_z_tied1, svuint8_t, uint64_t,
		 z0 = svlsl_wide_n_u8_z (p0, z0, x0),
		 z0 = svlsl_wide_z (p0, z0, x0))

/*
** lsl_wide_x0_u8_z_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.b, p0/z, z1\.b
**	lsl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_wide_x0_u8_z_untied, svuint8_t, uint64_t,
		 z0 = svlsl_wide_n_u8_z (p0, z1, x0),
		 z0 = svlsl_wide_z (p0, z1, x0))

/*
** lsl_wide_1_u8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	lsl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_1_u8_z_tied1, svuint8_t,
		z0 = svlsl_wide_n_u8_z (p0, z0, 1),
		z0 = svlsl_wide_z (p0, z0, 1))

/*
** lsl_wide_1_u8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	lsl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_1_u8_z_untied, svuint8_t,
		z0 = svlsl_wide_n_u8_z (p0, z1, 1),
		z0 = svlsl_wide_z (p0, z1, 1))

/*
** lsl_wide_7_u8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	lsl	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_7_u8_z_tied1, svuint8_t,
		z0 = svlsl_wide_n_u8_z (p0, z0, 7),
		z0 = svlsl_wide_z (p0, z0, 7))

/*
** lsl_wide_7_u8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	lsl	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_7_u8_z_untied, svuint8_t,
		z0 = svlsl_wide_n_u8_z (p0, z1, 7),
		z0 = svlsl_wide_z (p0, z1, 7))

/*
** lsl_wide_8_u8_z_tied1:
**	mov	(z[0-9]+\.d), #8
**	movprfx	z0\.b, p0/z, z0\.b
**	lsl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_8_u8_z_tied1, svuint8_t,
		z0 = svlsl_wide_n_u8_z (p0, z0, 8),
		z0 = svlsl_wide_z (p0, z0, 8))

/*
** lsl_wide_8_u8_z_untied:
**	mov	(z[0-9]+\.d), #8
**	movprfx	z0\.b, p0/z, z1\.b
**	lsl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_8_u8_z_untied, svuint8_t,
		z0 = svlsl_wide_n_u8_z (p0, z1, 8),
		z0 = svlsl_wide_z (p0, z1, 8))

/*
** lsl_wide_u8_x_tied1:
**	lsl	z0\.b, z0\.b, z4\.d
**	ret
*/
TEST_DUAL_Z (lsl_wide_u8_x_tied1, svuint8_t, svuint64_t,
	     z0 = svlsl_wide_u8_x (p0, z0, z4),
	     z0 = svlsl_wide_x (p0, z0, z4))

/*
** lsl_wide_u8_x_tied2:
**	lsl	z0\.b, z4\.b, z0\.d
**	ret
*/
TEST_DUAL_Z_REV (lsl_wide_u8_x_tied2, svuint8_t, svuint64_t,
		 z0_res = svlsl_wide_u8_x (p0, z4, z0),
		 z0_res = svlsl_wide_x (p0, z4, z0))

/*
** lsl_wide_u8_x_untied:
**	lsl	z0\.b, z1\.b, z4\.d
**	ret
*/
TEST_DUAL_Z (lsl_wide_u8_x_untied, svuint8_t, svuint64_t,
	     z0 = svlsl_wide_u8_x (p0, z1, z4),
	     z0 = svlsl_wide_x (p0, z1, z4))

/*
** lsl_wide_x0_u8_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	lsl	z0\.b, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_wide_x0_u8_x_tied1, svuint8_t, uint64_t,
		 z0 = svlsl_wide_n_u8_x (p0, z0, x0),
		 z0 = svlsl_wide_x (p0, z0, x0))

/*
** lsl_wide_x0_u8_x_untied:
**	mov	(z[0-9]+\.d), x0
**	lsl	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_wide_x0_u8_x_untied, svuint8_t, uint64_t,
		 z0 = svlsl_wide_n_u8_x (p0, z1, x0),
		 z0 = svlsl_wide_x (p0, z1, x0))

/*
** lsl_wide_1_u8_x_tied1:
**	lsl	z0\.b, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_1_u8_x_tied1, svuint8_t,
		z0 = svlsl_wide_n_u8_x (p0, z0, 1),
		z0 = svlsl_wide_x (p0, z0, 1))

/*
** lsl_wide_1_u8_x_untied:
**	lsl	z0\.b, z1\.b, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_1_u8_x_untied, svuint8_t,
		z0 = svlsl_wide_n_u8_x (p0, z1, 1),
		z0 = svlsl_wide_x (p0, z1, 1))

/*
** lsl_wide_7_u8_x_tied1:
**	lsl	z0\.b, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_7_u8_x_tied1, svuint8_t,
		z0 = svlsl_wide_n_u8_x (p0, z0, 7),
		z0 = svlsl_wide_x (p0, z0, 7))

/*
** lsl_wide_7_u8_x_untied:
**	lsl	z0\.b, z1\.b, #7
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_7_u8_x_untied, svuint8_t,
		z0 = svlsl_wide_n_u8_x (p0, z1, 7),
		z0 = svlsl_wide_x (p0, z1, 7))

/*
** lsl_wide_8_u8_x_tied1:
**	mov	(z[0-9]+\.d), #8
**	lsl	z0\.b, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_8_u8_x_tied1, svuint8_t,
		z0 = svlsl_wide_n_u8_x (p0, z0, 8),
		z0 = svlsl_wide_x (p0, z0, 8))

/*
** lsl_wide_8_u8_x_untied:
**	mov	(z[0-9]+\.d), #8
**	lsl	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_8_u8_x_untied, svuint8_t,
		z0 = svlsl_wide_n_u8_x (p0, z1, 8),
		z0 = svlsl_wide_x (p0, z1, 8))
