/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** lsl_u8_m_tied1:
**	lsl	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (lsl_u8_m_tied1, svuint8_t,
		z0 = svlsl_u8_m (p0, z0, z1),
		z0 = svlsl_m (p0, z0, z1))

/*
** lsl_u8_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	lsl	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_UNIFORM_Z (lsl_u8_m_tied2, svuint8_t,
		z0 = svlsl_u8_m (p0, z1, z0),
		z0 = svlsl_m (p0, z1, z0))

/*
** lsl_u8_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.b, p0/m, z0\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (lsl_u8_m_untied, svuint8_t,
		z0 = svlsl_u8_m (p0, z1, z2),
		z0 = svlsl_m (p0, z1, z2))

/*
** lsl_w0_u8_m_tied1:
**	mov	(z[0-9]+\.b), w0
**	lsl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_w0_u8_m_tied1, svuint8_t, uint8_t,
		 z0 = svlsl_n_u8_m (p0, z0, x0),
		 z0 = svlsl_m (p0, z0, x0))

/*
** lsl_w0_u8_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	lsl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_w0_u8_m_untied, svuint8_t, uint8_t,
		 z0 = svlsl_n_u8_m (p0, z1, x0),
		 z0 = svlsl_m (p0, z1, x0))

/*
** lsl_1_u8_m_tied1:
**	lsl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_1_u8_m_tied1, svuint8_t,
		z0 = svlsl_n_u8_m (p0, z0, 1),
		z0 = svlsl_m (p0, z0, 1))

/*
** lsl_1_u8_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_1_u8_m_untied, svuint8_t,
		z0 = svlsl_n_u8_m (p0, z1, 1),
		z0 = svlsl_m (p0, z1, 1))

/*
** lsl_7_u8_m_tied1:
**	lsl	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (lsl_7_u8_m_tied1, svuint8_t,
		z0 = svlsl_n_u8_m (p0, z0, 7),
		z0 = svlsl_m (p0, z0, 7))

/*
** lsl_7_u8_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (lsl_7_u8_m_untied, svuint8_t,
		z0 = svlsl_n_u8_m (p0, z1, 7),
		z0 = svlsl_m (p0, z1, 7))

/*
** lsl_8_u8_m_tied1:
**	mov	(z[0-9]+\.b), #8
**	lsl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_8_u8_m_tied1, svuint8_t,
		z0 = svlsl_n_u8_m (p0, z0, 8),
		z0 = svlsl_m (p0, z0, 8))

/*
** lsl_8_u8_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.b), #8
**	movprfx	z0, z1
**	lsl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_8_u8_m_untied, svuint8_t,
		z0 = svlsl_n_u8_m (p0, z1, 8),
		z0 = svlsl_m (p0, z1, 8))

/*
** lsl_u8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	lsl	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (lsl_u8_z_tied1, svuint8_t,
		z0 = svlsl_u8_z (p0, z0, z1),
		z0 = svlsl_z (p0, z0, z1))

/*
** lsl_u8_z_tied2:
**	movprfx	z0\.b, p0/z, z0\.b
**	lslr	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (lsl_u8_z_tied2, svuint8_t,
		z0 = svlsl_u8_z (p0, z1, z0),
		z0 = svlsl_z (p0, z1, z0))

/*
** lsl_u8_z_untied:
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	lsl	z0\.b, p0/m, z0\.b, z2\.b
** |
**	movprfx	z0\.b, p0/z, z2\.b
**	lslr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (lsl_u8_z_untied, svuint8_t,
		z0 = svlsl_u8_z (p0, z1, z2),
		z0 = svlsl_z (p0, z1, z2))

/*
** lsl_w0_u8_z_tied1:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0\.b, p0/z, z0\.b
**	lsl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_w0_u8_z_tied1, svuint8_t, uint8_t,
		 z0 = svlsl_n_u8_z (p0, z0, x0),
		 z0 = svlsl_z (p0, z0, x0))

/*
** lsl_w0_u8_z_untied:
**	mov	(z[0-9]+\.b), w0
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	lsl	z0\.b, p0/m, z0\.b, \1
** |
**	movprfx	z0\.b, p0/z, \1
**	lslr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_ZX (lsl_w0_u8_z_untied, svuint8_t, uint8_t,
		 z0 = svlsl_n_u8_z (p0, z1, x0),
		 z0 = svlsl_z (p0, z1, x0))

/*
** lsl_1_u8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	lsl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_1_u8_z_tied1, svuint8_t,
		z0 = svlsl_n_u8_z (p0, z0, 1),
		z0 = svlsl_z (p0, z0, 1))

/*
** lsl_1_u8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	lsl	z0\.b, p0/m, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_1_u8_z_untied, svuint8_t,
		z0 = svlsl_n_u8_z (p0, z1, 1),
		z0 = svlsl_z (p0, z1, 1))

/*
** lsl_7_u8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	lsl	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (lsl_7_u8_z_tied1, svuint8_t,
		z0 = svlsl_n_u8_z (p0, z0, 7),
		z0 = svlsl_z (p0, z0, 7))

/*
** lsl_7_u8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	lsl	z0\.b, p0/m, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (lsl_7_u8_z_untied, svuint8_t,
		z0 = svlsl_n_u8_z (p0, z1, 7),
		z0 = svlsl_z (p0, z1, 7))

/*
** lsl_8_u8_z_tied1:
**	mov	(z[0-9]+\.b), #8
**	movprfx	z0\.b, p0/z, z0\.b
**	lsl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_8_u8_z_tied1, svuint8_t,
		z0 = svlsl_n_u8_z (p0, z0, 8),
		z0 = svlsl_z (p0, z0, 8))

/*
** lsl_8_u8_z_untied:
**	mov	(z[0-9]+\.b), #8
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	lsl	z0\.b, p0/m, z0\.b, \1
** |
**	movprfx	z0\.b, p0/z, \1
**	lslr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (lsl_8_u8_z_untied, svuint8_t,
		z0 = svlsl_n_u8_z (p0, z1, 8),
		z0 = svlsl_z (p0, z1, 8))

/*
** lsl_u8_x_tied1:
**	lsl	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (lsl_u8_x_tied1, svuint8_t,
		z0 = svlsl_u8_x (p0, z0, z1),
		z0 = svlsl_x (p0, z0, z1))

/*
** lsl_u8_x_tied2:
**	lslr	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (lsl_u8_x_tied2, svuint8_t,
		z0 = svlsl_u8_x (p0, z1, z0),
		z0 = svlsl_x (p0, z1, z0))

/*
** lsl_u8_x_untied:
** (
**	movprfx	z0, z1
**	lsl	z0\.b, p0/m, z0\.b, z2\.b
** |
**	movprfx	z0, z2
**	lslr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (lsl_u8_x_untied, svuint8_t,
		z0 = svlsl_u8_x (p0, z1, z2),
		z0 = svlsl_x (p0, z1, z2))

/*
** lsl_w0_u8_x_tied1:
**	mov	(z[0-9]+\.b), w0
**	lsl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (lsl_w0_u8_x_tied1, svuint8_t, uint8_t,
		 z0 = svlsl_n_u8_x (p0, z0, x0),
		 z0 = svlsl_x (p0, z0, x0))

/*
** lsl_w0_u8_x_untied:
**	mov	z0\.b, w0
**	lslr	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_ZX (lsl_w0_u8_x_untied, svuint8_t, uint8_t,
		 z0 = svlsl_n_u8_x (p0, z1, x0),
		 z0 = svlsl_x (p0, z1, x0))

/*
** lsl_1_u8_x_tied1:
**	lsl	z0\.b, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_1_u8_x_tied1, svuint8_t,
		z0 = svlsl_n_u8_x (p0, z0, 1),
		z0 = svlsl_x (p0, z0, 1))

/*
** lsl_1_u8_x_untied:
**	lsl	z0\.b, z1\.b, #1
**	ret
*/
TEST_UNIFORM_Z (lsl_1_u8_x_untied, svuint8_t,
		z0 = svlsl_n_u8_x (p0, z1, 1),
		z0 = svlsl_x (p0, z1, 1))

/*
** lsl_7_u8_x_tied1:
**	lsl	z0\.b, z0\.b, #7
**	ret
*/
TEST_UNIFORM_Z (lsl_7_u8_x_tied1, svuint8_t,
		z0 = svlsl_n_u8_x (p0, z0, 7),
		z0 = svlsl_x (p0, z0, 7))

/*
** lsl_7_u8_x_untied:
**	lsl	z0\.b, z1\.b, #7
**	ret
*/
TEST_UNIFORM_Z (lsl_7_u8_x_untied, svuint8_t,
		z0 = svlsl_n_u8_x (p0, z1, 7),
		z0 = svlsl_x (p0, z1, 7))

/*
** lsl_8_u8_x_tied1:
**	mov	(z[0-9]+\.b), #8
**	lsl	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (lsl_8_u8_x_tied1, svuint8_t,
		z0 = svlsl_n_u8_x (p0, z0, 8),
		z0 = svlsl_x (p0, z0, 8))

/*
** lsl_8_u8_x_untied:
**	mov	z0\.b, #8
**	lslr	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (lsl_8_u8_x_untied, svuint8_t,
		z0 = svlsl_n_u8_x (p0, z1, 8),
		z0 = svlsl_x (p0, z1, 8))
