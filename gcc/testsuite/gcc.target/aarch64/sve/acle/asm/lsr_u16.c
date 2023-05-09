/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lsr_u16_m_tied1:
**	lsr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (lsr_u16_m_tied1, svuint16_t,
		z0 = svlsr_u16_m (p0, z0, z1),
		z0 = svlsr_m (p0, z0, z1))

/*
** lsr_u16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	lsr	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (lsr_u16_m_tied2, svuint16_t,
		z0 = svlsr_u16_m (p0, z1, z0),
		z0 = svlsr_m (p0, z1, z0))

/*
** lsr_u16_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (lsr_u16_m_untied, svuint16_t,
		z0 = svlsr_u16_m (p0, z1, z2),
		z0 = svlsr_m (p0, z1, z2))

/*
** lsr_w0_u16_m_tied1:
**	mov	(z[0-9]+\.h), w0
**	lsr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (lsr_w0_u16_m_tied1, svuint16_t, uint16_t,
		 z0 = svlsr_n_u16_m (p0, z0, x0),
		 z0 = svlsr_m (p0, z0, x0))

/*
** lsr_w0_u16_m_untied:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	lsr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (lsr_w0_u16_m_untied, svuint16_t, uint16_t,
		 z0 = svlsr_n_u16_m (p0, z1, x0),
		 z0 = svlsr_m (p0, z1, x0))

/*
** lsr_1_u16_m_tied1:
**	lsr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_1_u16_m_tied1, svuint16_t,
		z0 = svlsr_n_u16_m (p0, z0, 1),
		z0 = svlsr_m (p0, z0, 1))

/*
** lsr_1_u16_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_1_u16_m_untied, svuint16_t,
		z0 = svlsr_n_u16_m (p0, z1, 1),
		z0 = svlsr_m (p0, z1, 1))

/*
** lsr_15_u16_m_tied1:
**	lsr	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (lsr_15_u16_m_tied1, svuint16_t,
		z0 = svlsr_n_u16_m (p0, z0, 15),
		z0 = svlsr_m (p0, z0, 15))

/*
** lsr_15_u16_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (lsr_15_u16_m_untied, svuint16_t,
		z0 = svlsr_n_u16_m (p0, z1, 15),
		z0 = svlsr_m (p0, z1, 15))

/*
** lsr_16_u16_m_tied1:
**	lsr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (lsr_16_u16_m_tied1, svuint16_t,
		z0 = svlsr_n_u16_m (p0, z0, 16),
		z0 = svlsr_m (p0, z0, 16))

/*
** lsr_16_u16_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (lsr_16_u16_m_untied, svuint16_t,
		z0 = svlsr_n_u16_m (p0, z1, 16),
		z0 = svlsr_m (p0, z1, 16))

/*
** lsr_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (lsr_u16_z_tied1, svuint16_t,
		z0 = svlsr_u16_z (p0, z0, z1),
		z0 = svlsr_z (p0, z0, z1))

/*
** lsr_u16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsrr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (lsr_u16_z_tied2, svuint16_t,
		z0 = svlsr_u16_z (p0, z1, z0),
		z0 = svlsr_z (p0, z1, z0))

/*
** lsr_u16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	lsr	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0\.h, p0/z, z2\.h
**	lsrr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (lsr_u16_z_untied, svuint16_t,
		z0 = svlsr_u16_z (p0, z1, z2),
		z0 = svlsr_z (p0, z1, z2))

/*
** lsr_w0_u16_z_tied1:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	lsr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (lsr_w0_u16_z_tied1, svuint16_t, uint16_t,
		 z0 = svlsr_n_u16_z (p0, z0, x0),
		 z0 = svlsr_z (p0, z0, x0))

/*
** lsr_w0_u16_z_untied:
**	mov	(z[0-9]+\.h), w0
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	lsr	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	lsrr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZX (lsr_w0_u16_z_untied, svuint16_t, uint16_t,
		 z0 = svlsr_n_u16_z (p0, z1, x0),
		 z0 = svlsr_z (p0, z1, x0))

/*
** lsr_1_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_1_u16_z_tied1, svuint16_t,
		z0 = svlsr_n_u16_z (p0, z0, 1),
		z0 = svlsr_z (p0, z0, 1))

/*
** lsr_1_u16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	lsr	z0\.h, p0/m, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_1_u16_z_untied, svuint16_t,
		z0 = svlsr_n_u16_z (p0, z1, 1),
		z0 = svlsr_z (p0, z1, 1))

/*
** lsr_15_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsr	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (lsr_15_u16_z_tied1, svuint16_t,
		z0 = svlsr_n_u16_z (p0, z0, 15),
		z0 = svlsr_z (p0, z0, 15))

/*
** lsr_15_u16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	lsr	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (lsr_15_u16_z_untied, svuint16_t,
		z0 = svlsr_n_u16_z (p0, z1, 15),
		z0 = svlsr_z (p0, z1, 15))

/*
** lsr_16_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (lsr_16_u16_z_tied1, svuint16_t,
		z0 = svlsr_n_u16_z (p0, z0, 16),
		z0 = svlsr_z (p0, z0, 16))

/*
** lsr_16_u16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	lsr	z0\.h, p0/m, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (lsr_16_u16_z_untied, svuint16_t,
		z0 = svlsr_n_u16_z (p0, z1, 16),
		z0 = svlsr_z (p0, z1, 16))

/*
** lsr_u16_x_tied1:
**	lsr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (lsr_u16_x_tied1, svuint16_t,
		z0 = svlsr_u16_x (p0, z0, z1),
		z0 = svlsr_x (p0, z0, z1))

/*
** lsr_u16_x_tied2:
**	lsrr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (lsr_u16_x_tied2, svuint16_t,
		z0 = svlsr_u16_x (p0, z1, z0),
		z0 = svlsr_x (p0, z1, z0))

/*
** lsr_u16_x_untied:
** (
**	movprfx	z0, z1
**	lsr	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0, z2
**	lsrr	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (lsr_u16_x_untied, svuint16_t,
		z0 = svlsr_u16_x (p0, z1, z2),
		z0 = svlsr_x (p0, z1, z2))

/*
** lsr_w0_u16_x_tied1:
**	mov	(z[0-9]+\.h), w0
**	lsr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (lsr_w0_u16_x_tied1, svuint16_t, uint16_t,
		 z0 = svlsr_n_u16_x (p0, z0, x0),
		 z0 = svlsr_x (p0, z0, x0))

/*
** lsr_w0_u16_x_untied:
**	mov	z0\.h, w0
**	lsrr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZX (lsr_w0_u16_x_untied, svuint16_t, uint16_t,
		 z0 = svlsr_n_u16_x (p0, z1, x0),
		 z0 = svlsr_x (p0, z1, x0))

/*
** lsr_1_u16_x_tied1:
**	lsr	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_1_u16_x_tied1, svuint16_t,
		z0 = svlsr_n_u16_x (p0, z0, 1),
		z0 = svlsr_x (p0, z0, 1))

/*
** lsr_1_u16_x_untied:
**	lsr	z0\.h, z1\.h, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_1_u16_x_untied, svuint16_t,
		z0 = svlsr_n_u16_x (p0, z1, 1),
		z0 = svlsr_x (p0, z1, 1))

/*
** lsr_15_u16_x_tied1:
**	lsr	z0\.h, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (lsr_15_u16_x_tied1, svuint16_t,
		z0 = svlsr_n_u16_x (p0, z0, 15),
		z0 = svlsr_x (p0, z0, 15))

/*
** lsr_15_u16_x_untied:
**	lsr	z0\.h, z1\.h, #15
**	ret
*/
TEST_UNIFORM_Z (lsr_15_u16_x_untied, svuint16_t,
		z0 = svlsr_n_u16_x (p0, z1, 15),
		z0 = svlsr_x (p0, z1, 15))

/*
** lsr_16_u16_x_tied1:
**	lsr	z0\.h, z0\.h, #16
**	ret
*/
TEST_UNIFORM_Z (lsr_16_u16_x_tied1, svuint16_t,
		z0 = svlsr_n_u16_x (p0, z0, 16),
		z0 = svlsr_x (p0, z0, 16))

/*
** lsr_16_u16_x_untied:
**	lsr	z0\.h, z1\.h, #16
**	ret
*/
TEST_UNIFORM_Z (lsr_16_u16_x_untied, svuint16_t,
		z0 = svlsr_n_u16_x (p0, z1, 16),
		z0 = svlsr_x (p0, z1, 16))
