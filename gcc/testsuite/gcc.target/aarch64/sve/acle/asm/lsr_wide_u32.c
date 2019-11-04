/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lsr_wide_u32_m_tied1:
**	lsr	z0\.s, p0/m, z0\.s, z4\.d
**	ret
*/
TEST_DUAL_Z (lsr_wide_u32_m_tied1, svuint32_t, svuint64_t,
	     z0 = svlsr_wide_u32_m (p0, z0, z4),
	     z0 = svlsr_wide_m (p0, z0, z4))

/*
** lsr_wide_u32_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z4
**	lsr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_DUAL_Z_REV (lsr_wide_u32_m_tied2, svuint32_t, svuint64_t,
		 z0_res = svlsr_wide_u32_m (p0, z4, z0),
		 z0_res = svlsr_wide_m (p0, z4, z0))

/*
** lsr_wide_u32_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.s, p0/m, z0\.s, z4\.d
**	ret
*/
TEST_DUAL_Z (lsr_wide_u32_m_untied, svuint32_t, svuint64_t,
	     z0 = svlsr_wide_u32_m (p0, z1, z4),
	     z0 = svlsr_wide_m (p0, z1, z4))

/*
** lsr_wide_x0_u32_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	lsr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (lsr_wide_x0_u32_m_tied1, svuint32_t, uint64_t,
		 z0 = svlsr_wide_n_u32_m (p0, z0, x0),
		 z0 = svlsr_wide_m (p0, z0, x0))

/*
** lsr_wide_x0_u32_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	lsr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (lsr_wide_x0_u32_m_untied, svuint32_t, uint64_t,
		 z0 = svlsr_wide_n_u32_m (p0, z1, x0),
		 z0 = svlsr_wide_m (p0, z1, x0))

/*
** lsr_wide_1_u32_m_tied1:
**	lsr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_1_u32_m_tied1, svuint32_t,
		z0 = svlsr_wide_n_u32_m (p0, z0, 1),
		z0 = svlsr_wide_m (p0, z0, 1))

/*
** lsr_wide_1_u32_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_1_u32_m_untied, svuint32_t,
		z0 = svlsr_wide_n_u32_m (p0, z1, 1),
		z0 = svlsr_wide_m (p0, z1, 1))

/*
** lsr_wide_31_u32_m_tied1:
**	lsr	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_31_u32_m_tied1, svuint32_t,
		z0 = svlsr_wide_n_u32_m (p0, z0, 31),
		z0 = svlsr_wide_m (p0, z0, 31))

/*
** lsr_wide_31_u32_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_31_u32_m_untied, svuint32_t,
		z0 = svlsr_wide_n_u32_m (p0, z1, 31),
		z0 = svlsr_wide_m (p0, z1, 31))

/*
** lsr_wide_32_u32_m_tied1:
**	lsr	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_32_u32_m_tied1, svuint32_t,
		z0 = svlsr_wide_n_u32_m (p0, z0, 32),
		z0 = svlsr_wide_m (p0, z0, 32))

/*
** lsr_wide_32_u32_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_32_u32_m_untied, svuint32_t,
		z0 = svlsr_wide_n_u32_m (p0, z1, 32),
		z0 = svlsr_wide_m (p0, z1, 32))

/*
** lsr_wide_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsr	z0\.s, p0/m, z0\.s, z4\.d
**	ret
*/
TEST_DUAL_Z (lsr_wide_u32_z_tied1, svuint32_t, svuint64_t,
	     z0 = svlsr_wide_u32_z (p0, z0, z4),
	     z0 = svlsr_wide_z (p0, z0, z4))

/*
** lsr_wide_u32_z_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.s, p0/z, z4\.s
**	lsr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_DUAL_Z_REV (lsr_wide_u32_z_tied2, svuint32_t, svuint64_t,
		 z0_res = svlsr_wide_u32_z (p0, z4, z0),
		 z0_res = svlsr_wide_z (p0, z4, z0))

/*
** lsr_wide_u32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	lsr	z0\.s, p0/m, z0\.s, z4\.d
**	ret
*/
TEST_DUAL_Z (lsr_wide_u32_z_untied, svuint32_t, svuint64_t,
	     z0 = svlsr_wide_u32_z (p0, z1, z4),
	     z0 = svlsr_wide_z (p0, z1, z4))

/*
** lsr_wide_x0_u32_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.s, p0/z, z0\.s
**	lsr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (lsr_wide_x0_u32_z_tied1, svuint32_t, uint64_t,
		 z0 = svlsr_wide_n_u32_z (p0, z0, x0),
		 z0 = svlsr_wide_z (p0, z0, x0))

/*
** lsr_wide_x0_u32_z_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.s, p0/z, z1\.s
**	lsr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (lsr_wide_x0_u32_z_untied, svuint32_t, uint64_t,
		 z0 = svlsr_wide_n_u32_z (p0, z1, x0),
		 z0 = svlsr_wide_z (p0, z1, x0))

/*
** lsr_wide_1_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_1_u32_z_tied1, svuint32_t,
		z0 = svlsr_wide_n_u32_z (p0, z0, 1),
		z0 = svlsr_wide_z (p0, z0, 1))

/*
** lsr_wide_1_u32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	lsr	z0\.s, p0/m, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_1_u32_z_untied, svuint32_t,
		z0 = svlsr_wide_n_u32_z (p0, z1, 1),
		z0 = svlsr_wide_z (p0, z1, 1))

/*
** lsr_wide_31_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsr	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_31_u32_z_tied1, svuint32_t,
		z0 = svlsr_wide_n_u32_z (p0, z0, 31),
		z0 = svlsr_wide_z (p0, z0, 31))

/*
** lsr_wide_31_u32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	lsr	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_31_u32_z_untied, svuint32_t,
		z0 = svlsr_wide_n_u32_z (p0, z1, 31),
		z0 = svlsr_wide_z (p0, z1, 31))

/*
** lsr_wide_32_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsr	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_32_u32_z_tied1, svuint32_t,
		z0 = svlsr_wide_n_u32_z (p0, z0, 32),
		z0 = svlsr_wide_z (p0, z0, 32))

/*
** lsr_wide_32_u32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	lsr	z0\.s, p0/m, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_32_u32_z_untied, svuint32_t,
		z0 = svlsr_wide_n_u32_z (p0, z1, 32),
		z0 = svlsr_wide_z (p0, z1, 32))

/*
** lsr_wide_u32_x_tied1:
**	lsr	z0\.s, z0\.s, z4\.d
**	ret
*/
TEST_DUAL_Z (lsr_wide_u32_x_tied1, svuint32_t, svuint64_t,
	     z0 = svlsr_wide_u32_x (p0, z0, z4),
	     z0 = svlsr_wide_x (p0, z0, z4))

/*
** lsr_wide_u32_x_tied2:
**	lsr	z0\.s, z4\.s, z0\.d
**	ret
*/
TEST_DUAL_Z_REV (lsr_wide_u32_x_tied2, svuint32_t, svuint64_t,
		 z0_res = svlsr_wide_u32_x (p0, z4, z0),
		 z0_res = svlsr_wide_x (p0, z4, z0))

/*
** lsr_wide_u32_x_untied:
**	lsr	z0\.s, z1\.s, z4\.d
**	ret
*/
TEST_DUAL_Z (lsr_wide_u32_x_untied, svuint32_t, svuint64_t,
	     z0 = svlsr_wide_u32_x (p0, z1, z4),
	     z0 = svlsr_wide_x (p0, z1, z4))

/*
** lsr_wide_x0_u32_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	lsr	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (lsr_wide_x0_u32_x_tied1, svuint32_t, uint64_t,
		 z0 = svlsr_wide_n_u32_x (p0, z0, x0),
		 z0 = svlsr_wide_x (p0, z0, x0))

/*
** lsr_wide_x0_u32_x_untied:
**	mov	(z[0-9]+\.d), x0
**	lsr	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (lsr_wide_x0_u32_x_untied, svuint32_t, uint64_t,
		 z0 = svlsr_wide_n_u32_x (p0, z1, x0),
		 z0 = svlsr_wide_x (p0, z1, x0))

/*
** lsr_wide_1_u32_x_tied1:
**	lsr	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_1_u32_x_tied1, svuint32_t,
		z0 = svlsr_wide_n_u32_x (p0, z0, 1),
		z0 = svlsr_wide_x (p0, z0, 1))

/*
** lsr_wide_1_u32_x_untied:
**	lsr	z0\.s, z1\.s, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_1_u32_x_untied, svuint32_t,
		z0 = svlsr_wide_n_u32_x (p0, z1, 1),
		z0 = svlsr_wide_x (p0, z1, 1))

/*
** lsr_wide_31_u32_x_tied1:
**	lsr	z0\.s, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_31_u32_x_tied1, svuint32_t,
		z0 = svlsr_wide_n_u32_x (p0, z0, 31),
		z0 = svlsr_wide_x (p0, z0, 31))

/*
** lsr_wide_31_u32_x_untied:
**	lsr	z0\.s, z1\.s, #31
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_31_u32_x_untied, svuint32_t,
		z0 = svlsr_wide_n_u32_x (p0, z1, 31),
		z0 = svlsr_wide_x (p0, z1, 31))

/*
** lsr_wide_32_u32_x_tied1:
**	lsr	z0\.s, z0\.s, #32
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_32_u32_x_tied1, svuint32_t,
		z0 = svlsr_wide_n_u32_x (p0, z0, 32),
		z0 = svlsr_wide_x (p0, z0, 32))

/*
** lsr_wide_32_u32_x_untied:
**	lsr	z0\.s, z1\.s, #32
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_32_u32_x_untied, svuint32_t,
		z0 = svlsr_wide_n_u32_x (p0, z1, 32),
		z0 = svlsr_wide_x (p0, z1, 32))
