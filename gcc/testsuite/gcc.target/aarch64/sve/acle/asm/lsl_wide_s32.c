/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lsl_wide_s32_m_tied1:
**	lsl	z0\.s, p0/m, z0\.s, z4\.d
**	ret
*/
TEST_DUAL_Z (lsl_wide_s32_m_tied1, svint32_t, svuint64_t,
	     z0 = svlsl_wide_s32_m (p0, z0, z4),
	     z0 = svlsl_wide_m (p0, z0, z4))

/*
** lsl_wide_s32_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z4
**	lsl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_DUAL_Z_REV (lsl_wide_s32_m_tied2, svint32_t, svuint64_t,
		 z0_res = svlsl_wide_s32_m (p0, z4, z0),
		 z0_res = svlsl_wide_m (p0, z4, z0))

/*
** lsl_wide_s32_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.s, p0/m, z0\.s, z4\.d
**	ret
*/
TEST_DUAL_Z (lsl_wide_s32_m_untied, svint32_t, svuint64_t,
	     z0 = svlsl_wide_s32_m (p0, z1, z4),
	     z0 = svlsl_wide_m (p0, z1, z4))

/*
** lsl_wide_x0_s32_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	lsl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_wide_x0_s32_m_tied1, svint32_t, uint64_t,
		 z0 = svlsl_wide_n_s32_m (p0, z0, x0),
		 z0 = svlsl_wide_m (p0, z0, x0))

/*
** lsl_wide_x0_s32_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	lsl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_wide_x0_s32_m_untied, svint32_t, uint64_t,
		 z0 = svlsl_wide_n_s32_m (p0, z1, x0),
		 z0 = svlsl_wide_m (p0, z1, x0))

/*
** lsl_wide_1_s32_m_tied1:
**	lsl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_1_s32_m_tied1, svint32_t,
		z0 = svlsl_wide_n_s32_m (p0, z0, 1),
		z0 = svlsl_wide_m (p0, z0, 1))

/*
** lsl_wide_1_s32_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_1_s32_m_untied, svint32_t,
		z0 = svlsl_wide_n_s32_m (p0, z1, 1),
		z0 = svlsl_wide_m (p0, z1, 1))

/*
** lsl_wide_31_s32_m_tied1:
**	lsl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_31_s32_m_tied1, svint32_t,
		z0 = svlsl_wide_n_s32_m (p0, z0, 31),
		z0 = svlsl_wide_m (p0, z0, 31))

/*
** lsl_wide_31_s32_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_31_s32_m_untied, svint32_t,
		z0 = svlsl_wide_n_s32_m (p0, z1, 31),
		z0 = svlsl_wide_m (p0, z1, 31))

/*
** lsl_wide_32_s32_m_tied1:
**	mov	(z[0-9]+\.d), #32
**	lsl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_32_s32_m_tied1, svint32_t,
		z0 = svlsl_wide_n_s32_m (p0, z0, 32),
		z0 = svlsl_wide_m (p0, z0, 32))

/*
** lsl_wide_32_s32_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.d), #32
**	movprfx	z0, z1
**	lsl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_32_s32_m_untied, svint32_t,
		z0 = svlsl_wide_n_s32_m (p0, z1, 32),
		z0 = svlsl_wide_m (p0, z1, 32))

/*
** lsl_wide_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsl	z0\.s, p0/m, z0\.s, z4\.d
**	ret
*/
TEST_DUAL_Z (lsl_wide_s32_z_tied1, svint32_t, svuint64_t,
	     z0 = svlsl_wide_s32_z (p0, z0, z4),
	     z0 = svlsl_wide_z (p0, z0, z4))

/*
** lsl_wide_s32_z_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.s, p0/z, z4\.s
**	lsl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_DUAL_Z_REV (lsl_wide_s32_z_tied2, svint32_t, svuint64_t,
		 z0_res = svlsl_wide_s32_z (p0, z4, z0),
		 z0_res = svlsl_wide_z (p0, z4, z0))

/*
** lsl_wide_s32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	lsl	z0\.s, p0/m, z0\.s, z4\.d
**	ret
*/
TEST_DUAL_Z (lsl_wide_s32_z_untied, svint32_t, svuint64_t,
	     z0 = svlsl_wide_s32_z (p0, z1, z4),
	     z0 = svlsl_wide_z (p0, z1, z4))

/*
** lsl_wide_x0_s32_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.s, p0/z, z0\.s
**	lsl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_wide_x0_s32_z_tied1, svint32_t, uint64_t,
		 z0 = svlsl_wide_n_s32_z (p0, z0, x0),
		 z0 = svlsl_wide_z (p0, z0, x0))

/*
** lsl_wide_x0_s32_z_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.s, p0/z, z1\.s
**	lsl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_wide_x0_s32_z_untied, svint32_t, uint64_t,
		 z0 = svlsl_wide_n_s32_z (p0, z1, x0),
		 z0 = svlsl_wide_z (p0, z1, x0))

/*
** lsl_wide_1_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_1_s32_z_tied1, svint32_t,
		z0 = svlsl_wide_n_s32_z (p0, z0, 1),
		z0 = svlsl_wide_z (p0, z0, 1))

/*
** lsl_wide_1_s32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	lsl	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_1_s32_z_untied, svint32_t,
		z0 = svlsl_wide_n_s32_z (p0, z1, 1),
		z0 = svlsl_wide_z (p0, z1, 1))

/*
** lsl_wide_31_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_31_s32_z_tied1, svint32_t,
		z0 = svlsl_wide_n_s32_z (p0, z0, 31),
		z0 = svlsl_wide_z (p0, z0, 31))

/*
** lsl_wide_31_s32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	lsl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_31_s32_z_untied, svint32_t,
		z0 = svlsl_wide_n_s32_z (p0, z1, 31),
		z0 = svlsl_wide_z (p0, z1, 31))

/*
** lsl_wide_32_s32_z_tied1:
**	mov	(z[0-9]+\.d), #32
**	movprfx	z0\.s, p0/z, z0\.s
**	lsl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_32_s32_z_tied1, svint32_t,
		z0 = svlsl_wide_n_s32_z (p0, z0, 32),
		z0 = svlsl_wide_z (p0, z0, 32))

/*
** lsl_wide_32_s32_z_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.d), #32
**	movprfx	z0\.s, p0/z, z1\.s
**	lsl	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_32_s32_z_untied, svint32_t,
		z0 = svlsl_wide_n_s32_z (p0, z1, 32),
		z0 = svlsl_wide_z (p0, z1, 32))

/*
** lsl_wide_s32_x_tied1:
**	lsl	z0\.s, z0\.s, z4\.d
**	ret
*/
TEST_DUAL_Z (lsl_wide_s32_x_tied1, svint32_t, svuint64_t,
	     z0 = svlsl_wide_s32_x (p0, z0, z4),
	     z0 = svlsl_wide_x (p0, z0, z4))

/*
** lsl_wide_s32_x_tied2:
**	lsl	z0\.s, z4\.s, z0\.d
**	ret
*/
TEST_DUAL_Z_REV (lsl_wide_s32_x_tied2, svint32_t, svuint64_t,
		 z0_res = svlsl_wide_s32_x (p0, z4, z0),
		 z0_res = svlsl_wide_x (p0, z4, z0))

/*
** lsl_wide_s32_x_untied:
**	lsl	z0\.s, z1\.s, z4\.d
**	ret
*/
TEST_DUAL_Z (lsl_wide_s32_x_untied, svint32_t, svuint64_t,
	     z0 = svlsl_wide_s32_x (p0, z1, z4),
	     z0 = svlsl_wide_x (p0, z1, z4))

/*
** lsl_wide_x0_s32_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	lsl	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_wide_x0_s32_x_tied1, svint32_t, uint64_t,
		 z0 = svlsl_wide_n_s32_x (p0, z0, x0),
		 z0 = svlsl_wide_x (p0, z0, x0))

/*
** lsl_wide_x0_s32_x_untied:
**	mov	(z[0-9]+\.d), x0
**	lsl	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_wide_x0_s32_x_untied, svint32_t, uint64_t,
		 z0 = svlsl_wide_n_s32_x (p0, z1, x0),
		 z0 = svlsl_wide_x (p0, z1, x0))

/*
** lsl_wide_1_s32_x_tied1:
**	lsl	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_1_s32_x_tied1, svint32_t,
		z0 = svlsl_wide_n_s32_x (p0, z0, 1),
		z0 = svlsl_wide_x (p0, z0, 1))

/*
** lsl_wide_1_s32_x_untied:
**	lsl	z0\.s, z1\.s, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_1_s32_x_untied, svint32_t,
		z0 = svlsl_wide_n_s32_x (p0, z1, 1),
		z0 = svlsl_wide_x (p0, z1, 1))

/*
** lsl_wide_31_s32_x_tied1:
**	lsl	z0\.s, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_31_s32_x_tied1, svint32_t,
		z0 = svlsl_wide_n_s32_x (p0, z0, 31),
		z0 = svlsl_wide_x (p0, z0, 31))

/*
** lsl_wide_31_s32_x_untied:
**	lsl	z0\.s, z1\.s, #31
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_31_s32_x_untied, svint32_t,
		z0 = svlsl_wide_n_s32_x (p0, z1, 31),
		z0 = svlsl_wide_x (p0, z1, 31))

/*
** lsl_wide_32_s32_x_tied1:
**	mov	(z[0-9]+\.d), #32
**	lsl	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_32_s32_x_tied1, svint32_t,
		z0 = svlsl_wide_n_s32_x (p0, z0, 32),
		z0 = svlsl_wide_x (p0, z0, 32))

/*
** lsl_wide_32_s32_x_untied:
**	mov	(z[0-9]+\.d), #32
**	lsl	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_wide_32_s32_x_untied, svint32_t,
		z0 = svlsl_wide_n_s32_x (p0, z1, 32),
		z0 = svlsl_wide_x (p0, z1, 32))
