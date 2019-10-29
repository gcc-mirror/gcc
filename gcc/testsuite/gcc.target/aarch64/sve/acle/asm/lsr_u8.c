/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lsr_u8_m_tied1:
**	lsr	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (lsr_u8_m_tied1, svuint8_t,
		z0 = svlsr_u8_m (p0, z0, z1),
		z0 = svlsr_m (p0, z0, z1))

/*
** lsr_u8_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	lsr	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_UNIFORM_Z (lsr_u8_m_tied2, svuint8_t,
		z0 = svlsr_u8_m (p0, z1, z0),
		z0 = svlsr_m (p0, z1, z0))

/*
** lsr_u8_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.b, p0/m, z0\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (lsr_u8_m_untied, svuint8_t,
		z0 = svlsr_u8_m (p0, z1, z2),
		z0 = svlsr_m (p0, z1, z2))

/*
** lsr_w0_u8_m_tied1:
**	mov	(z[0-9]+\.b), w0
**	lsr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (lsr_w0_u8_m_tied1, svuint8_t, uint8_t,
		 z0 = svlsr_n_u8_m (p0, z0, x0),
		 z0 = svlsr_m (p0, z0, x0))

/*
** lsr_w0_u8_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	lsr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (lsr_w0_u8_m_untied, svuint8_t, uint8_t,
		 z0 = svlsr_n_u8_m (p0, z1, x0),
		 z0 = svlsr_m (p0, z1, x0))

/*
** lsr_1_u8_m_tied1:
**	lsr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_1_u8_m_tied1, svuint8_t,
		z0 = svlsr_n_u8_m (p0, z0, 1),
		z0 = svlsr_m (p0, z0, 1))

/*
** lsr_1_u8_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_1_u8_m_untied, svuint8_t,
		z0 = svlsr_n_u8_m (p0, z1, 1),
		z0 = svlsr_m (p0, z1, 1))

/*
** lsr_7_u8_m_tied1:
**	lsr	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (lsr_7_u8_m_tied1, svuint8_t,
		z0 = svlsr_n_u8_m (p0, z0, 7),
		z0 = svlsr_m (p0, z0, 7))

/*
** lsr_7_u8_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (lsr_7_u8_m_untied, svuint8_t,
		z0 = svlsr_n_u8_m (p0, z1, 7),
		z0 = svlsr_m (p0, z1, 7))

/*
** lsr_8_u8_m_tied1:
**	lsr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (lsr_8_u8_m_tied1, svuint8_t,
		z0 = svlsr_n_u8_m (p0, z0, 8),
		z0 = svlsr_m (p0, z0, 8))

/*
** lsr_8_u8_m_untied:
**	movprfx	z0, z1
**	lsr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (lsr_8_u8_m_untied, svuint8_t,
		z0 = svlsr_n_u8_m (p0, z1, 8),
		z0 = svlsr_m (p0, z1, 8))

/*
** lsr_u8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	lsr	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (lsr_u8_z_tied1, svuint8_t,
		z0 = svlsr_u8_z (p0, z0, z1),
		z0 = svlsr_z (p0, z0, z1))

/*
** lsr_u8_z_tied2:
**	movprfx	z0\.b, p0/z, z0\.b
**	lsrr	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (lsr_u8_z_tied2, svuint8_t,
		z0 = svlsr_u8_z (p0, z1, z0),
		z0 = svlsr_z (p0, z1, z0))

/*
** lsr_u8_z_untied:
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	lsr	z0\.b, p0/m, z0\.b, z2\.b
** |
**	movprfx	z0\.b, p0/z, z2\.b
**	lsrr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (lsr_u8_z_untied, svuint8_t,
		z0 = svlsr_u8_z (p0, z1, z2),
		z0 = svlsr_z (p0, z1, z2))

/*
** lsr_w0_u8_z_tied1:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0\.b, p0/z, z0\.b
**	lsr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (lsr_w0_u8_z_tied1, svuint8_t, uint8_t,
		 z0 = svlsr_n_u8_z (p0, z0, x0),
		 z0 = svlsr_z (p0, z0, x0))

/*
** lsr_w0_u8_z_untied:
**	mov	(z[0-9]+\.b), w0
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	lsr	z0\.b, p0/m, z0\.b, \1
** |
**	movprfx	z0\.b, p0/z, \1
**	lsrr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_ZX (lsr_w0_u8_z_untied, svuint8_t, uint8_t,
		 z0 = svlsr_n_u8_z (p0, z1, x0),
		 z0 = svlsr_z (p0, z1, x0))

/*
** lsr_1_u8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	lsr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_1_u8_z_tied1, svuint8_t,
		z0 = svlsr_n_u8_z (p0, z0, 1),
		z0 = svlsr_z (p0, z0, 1))

/*
** lsr_1_u8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	lsr	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_1_u8_z_untied, svuint8_t,
		z0 = svlsr_n_u8_z (p0, z1, 1),
		z0 = svlsr_z (p0, z1, 1))

/*
** lsr_7_u8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	lsr	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (lsr_7_u8_z_tied1, svuint8_t,
		z0 = svlsr_n_u8_z (p0, z0, 7),
		z0 = svlsr_z (p0, z0, 7))

/*
** lsr_7_u8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	lsr	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (lsr_7_u8_z_untied, svuint8_t,
		z0 = svlsr_n_u8_z (p0, z1, 7),
		z0 = svlsr_z (p0, z1, 7))

/*
** lsr_8_u8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	lsr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (lsr_8_u8_z_tied1, svuint8_t,
		z0 = svlsr_n_u8_z (p0, z0, 8),
		z0 = svlsr_z (p0, z0, 8))

/*
** lsr_8_u8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	lsr	z0\.b, p0/m, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (lsr_8_u8_z_untied, svuint8_t,
		z0 = svlsr_n_u8_z (p0, z1, 8),
		z0 = svlsr_z (p0, z1, 8))

/*
** lsr_u8_x_tied1:
**	lsr	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (lsr_u8_x_tied1, svuint8_t,
		z0 = svlsr_u8_x (p0, z0, z1),
		z0 = svlsr_x (p0, z0, z1))

/*
** lsr_u8_x_tied2:
**	lsrr	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (lsr_u8_x_tied2, svuint8_t,
		z0 = svlsr_u8_x (p0, z1, z0),
		z0 = svlsr_x (p0, z1, z0))

/*
** lsr_u8_x_untied:
** (
**	movprfx	z0, z1
**	lsr	z0\.b, p0/m, z0\.b, z2\.b
** |
**	movprfx	z0, z2
**	lsrr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (lsr_u8_x_untied, svuint8_t,
		z0 = svlsr_u8_x (p0, z1, z2),
		z0 = svlsr_x (p0, z1, z2))

/*
** lsr_w0_u8_x_tied1:
**	mov	(z[0-9]+\.b), w0
**	lsr	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (lsr_w0_u8_x_tied1, svuint8_t, uint8_t,
		 z0 = svlsr_n_u8_x (p0, z0, x0),
		 z0 = svlsr_x (p0, z0, x0))

/*
** lsr_w0_u8_x_untied:
**	mov	z0\.b, w0
**	lsrr	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_ZX (lsr_w0_u8_x_untied, svuint8_t, uint8_t,
		 z0 = svlsr_n_u8_x (p0, z1, x0),
		 z0 = svlsr_x (p0, z1, x0))

/*
** lsr_1_u8_x_tied1:
**	lsr	z0\.b, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_1_u8_x_tied1, svuint8_t,
		z0 = svlsr_n_u8_x (p0, z0, 1),
		z0 = svlsr_x (p0, z0, 1))

/*
** lsr_1_u8_x_untied:
**	lsr	z0\.b, z1\.b, #1
**	ret
*/
TEST_UNIFORM_Z (lsr_1_u8_x_untied, svuint8_t,
		z0 = svlsr_n_u8_x (p0, z1, 1),
		z0 = svlsr_x (p0, z1, 1))

/*
** lsr_7_u8_x_tied1:
**	lsr	z0\.b, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (lsr_7_u8_x_tied1, svuint8_t,
		z0 = svlsr_n_u8_x (p0, z0, 7),
		z0 = svlsr_x (p0, z0, 7))

/*
** lsr_7_u8_x_untied:
**	lsr	z0\.b, z1\.b, #7
**	ret
*/
TEST_UNIFORM_Z (lsr_7_u8_x_untied, svuint8_t,
		z0 = svlsr_n_u8_x (p0, z1, 7),
		z0 = svlsr_x (p0, z1, 7))

/*
** lsr_8_u8_x_tied1:
**	lsr	z0\.b, z0\.b, #8
**	ret
*/
TEST_UNIFORM_Z (lsr_8_u8_x_tied1, svuint8_t,
		z0 = svlsr_n_u8_x (p0, z0, 8),
		z0 = svlsr_x (p0, z0, 8))

/*
** lsr_8_u8_x_untied:
**	lsr	z0\.b, z1\.b, #8
**	ret
*/
TEST_UNIFORM_Z (lsr_8_u8_x_untied, svuint8_t,
		z0 = svlsr_n_u8_x (p0, z1, 8),
		z0 = svlsr_x (p0, z1, 8))
