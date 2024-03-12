/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lsr_wide_u16_m_tied1:
**	lsr	z0\.h, p0/m, z0\.h, z4\.d
**	ret
*/
TEST_DUAL_Z (lsr_wide_u16_m_tied1, svuint16_t, svuint64_t,
	     z0 = svlsr_wide_u16_m (p0, z0, z4),
	     z0 = svlsr_wide_m (p0, z0, z4))

/*
** lsr_wide_u16_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z4
**	lsr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_DUAL_Z_REV (lsr_wide_u16_m_tied2, svuint16_t, svuint64_t,
		 z0_res = svlsr_wide_u16_m (p0, z4, z0),
		 z0_res = svlsr_wide_m (p0, z4, z0))

/*
** lsr_wide_u16_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.h, p0/m, z0\.h, z4\.d
**	ret
*/
TEST_DUAL_Z (lsr_wide_u16_m_untied, svuint16_t, svuint64_t,
	     z0 = svlsr_wide_u16_m (p0, z1, z4),
	     z0 = svlsr_wide_m (p0, z1, z4))

/*
** lsr_wide_x0_u16_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	lsr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (lsr_wide_x0_u16_m_tied1, svuint16_t, uint64_t,
		 z0 = svlsr_wide_n_u16_m (p0, z0, x0),
		 z0 = svlsr_wide_m (p0, z0, x0))

/*
** lsr_wide_x0_u16_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	lsr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (lsr_wide_x0_u16_m_untied, svuint16_t, uint64_t,
		 z0 = svlsr_wide_n_u16_m (p0, z1, x0),
		 z0 = svlsr_wide_m (p0, z1, x0))

/*
** lsr_wide_1_u16_m_tied1:
**	lsr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_1_u16_m_tied1, svuint16_t,
		z0 = svlsr_wide_n_u16_m (p0, z0, 1),
		z0 = svlsr_wide_m (p0, z0, 1))

/*
** lsr_wide_1_u16_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_1_u16_m_untied, svuint16_t,
		z0 = svlsr_wide_n_u16_m (p0, z1, 1),
		z0 = svlsr_wide_m (p0, z1, 1))

/*
** lsr_wide_15_u16_m_tied1:
**	lsr	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_15_u16_m_tied1, svuint16_t,
		z0 = svlsr_wide_n_u16_m (p0, z0, 15),
		z0 = svlsr_wide_m (p0, z0, 15))

/*
** lsr_wide_15_u16_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_15_u16_m_untied, svuint16_t,
		z0 = svlsr_wide_n_u16_m (p0, z1, 15),
		z0 = svlsr_wide_m (p0, z1, 15))

/*
** lsr_wide_16_u16_m_tied1:
**	lsr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_16_u16_m_tied1, svuint16_t,
		z0 = svlsr_wide_n_u16_m (p0, z0, 16),
		z0 = svlsr_wide_m (p0, z0, 16))

/*
** lsr_wide_16_u16_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_16_u16_m_untied, svuint16_t,
		z0 = svlsr_wide_n_u16_m (p0, z1, 16),
		z0 = svlsr_wide_m (p0, z1, 16))

/*
** lsr_wide_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsr	z0\.h, p0/m, z0\.h, z4\.d
**	ret
*/
TEST_DUAL_Z (lsr_wide_u16_z_tied1, svuint16_t, svuint64_t,
	     z0 = svlsr_wide_u16_z (p0, z0, z4),
	     z0 = svlsr_wide_z (p0, z0, z4))

/*
** lsr_wide_u16_z_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.h, p0/z, z4\.h
**	lsr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_DUAL_Z_REV (lsr_wide_u16_z_tied2, svuint16_t, svuint64_t,
		 z0_res = svlsr_wide_u16_z (p0, z4, z0),
		 z0_res = svlsr_wide_z (p0, z4, z0))

/*
** lsr_wide_u16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	lsr	z0\.h, p0/m, z0\.h, z4\.d
**	ret
*/
TEST_DUAL_Z (lsr_wide_u16_z_untied, svuint16_t, svuint64_t,
	     z0 = svlsr_wide_u16_z (p0, z1, z4),
	     z0 = svlsr_wide_z (p0, z1, z4))

/*
** lsr_wide_x0_u16_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.h, p0/z, z0\.h
**	lsr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (lsr_wide_x0_u16_z_tied1, svuint16_t, uint64_t,
		 z0 = svlsr_wide_n_u16_z (p0, z0, x0),
		 z0 = svlsr_wide_z (p0, z0, x0))

/*
** lsr_wide_x0_u16_z_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.h, p0/z, z1\.h
**	lsr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (lsr_wide_x0_u16_z_untied, svuint16_t, uint64_t,
		 z0 = svlsr_wide_n_u16_z (p0, z1, x0),
		 z0 = svlsr_wide_z (p0, z1, x0))

/*
** lsr_wide_1_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_1_u16_z_tied1, svuint16_t,
		z0 = svlsr_wide_n_u16_z (p0, z0, 1),
		z0 = svlsr_wide_z (p0, z0, 1))

/*
** lsr_wide_1_u16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	lsr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_1_u16_z_untied, svuint16_t,
		z0 = svlsr_wide_n_u16_z (p0, z1, 1),
		z0 = svlsr_wide_z (p0, z1, 1))

/*
** lsr_wide_15_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsr	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_15_u16_z_tied1, svuint16_t,
		z0 = svlsr_wide_n_u16_z (p0, z0, 15),
		z0 = svlsr_wide_z (p0, z0, 15))

/*
** lsr_wide_15_u16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	lsr	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_15_u16_z_untied, svuint16_t,
		z0 = svlsr_wide_n_u16_z (p0, z1, 15),
		z0 = svlsr_wide_z (p0, z1, 15))

/*
** lsr_wide_16_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_16_u16_z_tied1, svuint16_t,
		z0 = svlsr_wide_n_u16_z (p0, z0, 16),
		z0 = svlsr_wide_z (p0, z0, 16))

/*
** lsr_wide_16_u16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	lsr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_16_u16_z_untied, svuint16_t,
		z0 = svlsr_wide_n_u16_z (p0, z1, 16),
		z0 = svlsr_wide_z (p0, z1, 16))

/*
** lsr_wide_u16_x_tied1:
**	lsr	z0\.h, z0\.h, z4\.d
**	ret
*/
TEST_DUAL_Z (lsr_wide_u16_x_tied1, svuint16_t, svuint64_t,
	     z0 = svlsr_wide_u16_x (p0, z0, z4),
	     z0 = svlsr_wide_x (p0, z0, z4))

/*
** lsr_wide_u16_x_tied2:
**	lsr	z0\.h, z4\.h, z0\.d
**	ret
*/
TEST_DUAL_Z_REV (lsr_wide_u16_x_tied2, svuint16_t, svuint64_t,
		 z0_res = svlsr_wide_u16_x (p0, z4, z0),
		 z0_res = svlsr_wide_x (p0, z4, z0))

/*
** lsr_wide_u16_x_untied:
**	lsr	z0\.h, z1\.h, z4\.d
**	ret
*/
TEST_DUAL_Z (lsr_wide_u16_x_untied, svuint16_t, svuint64_t,
	     z0 = svlsr_wide_u16_x (p0, z1, z4),
	     z0 = svlsr_wide_x (p0, z1, z4))

/*
** lsr_wide_x0_u16_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	lsr	z0\.h, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (lsr_wide_x0_u16_x_tied1, svuint16_t, uint64_t,
		 z0 = svlsr_wide_n_u16_x (p0, z0, x0),
		 z0 = svlsr_wide_x (p0, z0, x0))

/*
** lsr_wide_x0_u16_x_untied:
**	mov	(z[0-9]+\.d), x0
**	lsr	z0\.h, z1\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (lsr_wide_x0_u16_x_untied, svuint16_t, uint64_t,
		 z0 = svlsr_wide_n_u16_x (p0, z1, x0),
		 z0 = svlsr_wide_x (p0, z1, x0))

/*
** lsr_wide_1_u16_x_tied1:
**	lsr	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_1_u16_x_tied1, svuint16_t,
		z0 = svlsr_wide_n_u16_x (p0, z0, 1),
		z0 = svlsr_wide_x (p0, z0, 1))

/*
** lsr_wide_1_u16_x_untied:
**	lsr	z0\.h, z1\.h, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_1_u16_x_untied, svuint16_t,
		z0 = svlsr_wide_n_u16_x (p0, z1, 1),
		z0 = svlsr_wide_x (p0, z1, 1))

/*
** lsr_wide_15_u16_x_tied1:
**	lsr	z0\.h, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_15_u16_x_tied1, svuint16_t,
		z0 = svlsr_wide_n_u16_x (p0, z0, 15),
		z0 = svlsr_wide_x (p0, z0, 15))

/*
** lsr_wide_15_u16_x_untied:
**	lsr	z0\.h, z1\.h, #15
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_15_u16_x_untied, svuint16_t,
		z0 = svlsr_wide_n_u16_x (p0, z1, 15),
		z0 = svlsr_wide_x (p0, z1, 15))

/*
** lsr_wide_16_u16_x_tied1:
**	lsr	z0\.h, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_16_u16_x_tied1, svuint16_t,
		z0 = svlsr_wide_n_u16_x (p0, z0, 16),
		z0 = svlsr_wide_x (p0, z0, 16))

/*
** lsr_wide_16_u16_x_untied:
**	lsr	z0\.h, z1\.h, #16
**	ret
*/
TEST_UNIFORM_Z (lsr_wide_16_u16_x_untied, svuint16_t,
		z0 = svlsr_wide_n_u16_x (p0, z1, 16),
		z0 = svlsr_wide_x (p0, z1, 16))
