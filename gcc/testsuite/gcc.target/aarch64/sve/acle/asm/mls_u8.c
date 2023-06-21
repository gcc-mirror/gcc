/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mls_u8_m_tied1:
**	mls	z0\.b, p0/m, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (mls_u8_m_tied1, svuint8_t,
		z0 = svmls_u8_m (p0, z0, z1, z2),
		z0 = svmls_m (p0, z0, z1, z2))

/*
** mls_u8_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	mls	z0\.b, p0/m, \1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (mls_u8_m_tied2, svuint8_t,
		z0 = svmls_u8_m (p0, z1, z0, z2),
		z0 = svmls_m (p0, z1, z0, z2))

/*
** mls_u8_m_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	mls	z0\.b, p0/m, z2\.b, \1\.b
**	ret
*/
TEST_UNIFORM_Z (mls_u8_m_tied3, svuint8_t,
		z0 = svmls_u8_m (p0, z1, z2, z0),
		z0 = svmls_m (p0, z1, z2, z0))

/*
** mls_u8_m_untied:
**	movprfx	z0, z1
**	mls	z0\.b, p0/m, z2\.b, z3\.b
**	ret
*/
TEST_UNIFORM_Z (mls_u8_m_untied, svuint8_t,
		z0 = svmls_u8_m (p0, z1, z2, z3),
		z0 = svmls_m (p0, z1, z2, z3))

/*
** mls_w0_u8_m_tied1:
**	mov	(z[0-9]+\.b), w0
**	mls	z0\.b, p0/m, z1\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (mls_w0_u8_m_tied1, svuint8_t, uint8_t,
		 z0 = svmls_n_u8_m (p0, z0, z1, x0),
		 z0 = svmls_m (p0, z0, z1, x0))

/*
** mls_w0_u8_m_untied:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	mls	z0\.b, p0/m, z2\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (mls_w0_u8_m_untied, svuint8_t, uint8_t,
		 z0 = svmls_n_u8_m (p0, z1, z2, x0),
		 z0 = svmls_m (p0, z1, z2, x0))

/*
** mls_11_u8_m_tied1:
**	mov	(z[0-9]+\.b), #11
**	mls	z0\.b, p0/m, z1\.b, \1
**	ret
*/
TEST_UNIFORM_Z (mls_11_u8_m_tied1, svuint8_t,
		z0 = svmls_n_u8_m (p0, z0, z1, 11),
		z0 = svmls_m (p0, z0, z1, 11))

/*
** mls_11_u8_m_untied:
**	mov	(z[0-9]+\.b), #11
**	movprfx	z0, z1
**	mls	z0\.b, p0/m, z2\.b, \1
**	ret
*/
TEST_UNIFORM_Z (mls_11_u8_m_untied, svuint8_t,
		z0 = svmls_n_u8_m (p0, z1, z2, 11),
		z0 = svmls_m (p0, z1, z2, 11))

/*
** mls_u8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	mls	z0\.b, p0/m, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (mls_u8_z_tied1, svuint8_t,
		z0 = svmls_u8_z (p0, z0, z1, z2),
		z0 = svmls_z (p0, z0, z1, z2))

/*
** mls_u8_z_tied2:
**	movprfx	z0\.b, p0/z, z0\.b
**	msb	z0\.b, p0/m, z2\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (mls_u8_z_tied2, svuint8_t,
		z0 = svmls_u8_z (p0, z1, z0, z2),
		z0 = svmls_z (p0, z1, z0, z2))

/*
** mls_u8_z_tied3:
**	movprfx	z0\.b, p0/z, z0\.b
**	msb	z0\.b, p0/m, z2\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (mls_u8_z_tied3, svuint8_t,
		z0 = svmls_u8_z (p0, z1, z2, z0),
		z0 = svmls_z (p0, z1, z2, z0))

/*
** mls_u8_z_untied:
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	mls	z0\.b, p0/m, z2\.b, z3\.b
** |
**	movprfx	z0\.b, p0/z, z2\.b
**	msb	z0\.b, p0/m, z3\.b, z1\.b
** |
**	movprfx	z0\.b, p0/z, z3\.b
**	msb	z0\.b, p0/m, z2\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (mls_u8_z_untied, svuint8_t,
		z0 = svmls_u8_z (p0, z1, z2, z3),
		z0 = svmls_z (p0, z1, z2, z3))

/*
** mls_w0_u8_z_tied1:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0\.b, p0/z, z0\.b
**	mls	z0\.b, p0/m, z1\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (mls_w0_u8_z_tied1, svuint8_t, uint8_t,
		 z0 = svmls_n_u8_z (p0, z0, z1, x0),
		 z0 = svmls_z (p0, z0, z1, x0))

/*
** mls_w0_u8_z_tied2:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0\.b, p0/z, z0\.b
**	msb	z0\.b, p0/m, \1, z1\.b
**	ret
*/
TEST_UNIFORM_ZX (mls_w0_u8_z_tied2, svuint8_t, uint8_t,
		 z0 = svmls_n_u8_z (p0, z1, z0, x0),
		 z0 = svmls_z (p0, z1, z0, x0))

/*
** mls_w0_u8_z_untied:
**	mov	(z[0-9]+\.b), w0
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	mls	z0\.b, p0/m, z2\.b, \1
** |
**	movprfx	z0\.b, p0/z, z2\.b
**	msb	z0\.b, p0/m, \1, z1\.b
** |
**	movprfx	z0\.b, p0/z, \1
**	msb	z0\.b, p0/m, z2\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_ZX (mls_w0_u8_z_untied, svuint8_t, uint8_t,
		 z0 = svmls_n_u8_z (p0, z1, z2, x0),
		 z0 = svmls_z (p0, z1, z2, x0))

/*
** mls_11_u8_z_tied1:
**	mov	(z[0-9]+\.b), #11
**	movprfx	z0\.b, p0/z, z0\.b
**	mls	z0\.b, p0/m, z1\.b, \1
**	ret
*/
TEST_UNIFORM_Z (mls_11_u8_z_tied1, svuint8_t,
		z0 = svmls_n_u8_z (p0, z0, z1, 11),
		z0 = svmls_z (p0, z0, z1, 11))

/*
** mls_11_u8_z_tied2:
**	mov	(z[0-9]+\.b), #11
**	movprfx	z0\.b, p0/z, z0\.b
**	msb	z0\.b, p0/m, \1, z1\.b
**	ret
*/
TEST_UNIFORM_Z (mls_11_u8_z_tied2, svuint8_t,
		z0 = svmls_n_u8_z (p0, z1, z0, 11),
		z0 = svmls_z (p0, z1, z0, 11))

/*
** mls_11_u8_z_untied:
**	mov	(z[0-9]+\.b), #11
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	mls	z0\.b, p0/m, z2\.b, \1
** |
**	movprfx	z0\.b, p0/z, z2\.b
**	msb	z0\.b, p0/m, \1, z1\.b
** |
**	movprfx	z0\.b, p0/z, \1
**	msb	z0\.b, p0/m, z2\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (mls_11_u8_z_untied, svuint8_t,
		z0 = svmls_n_u8_z (p0, z1, z2, 11),
		z0 = svmls_z (p0, z1, z2, 11))

/*
** mls_u8_x_tied1:
**	mls	z0\.b, p0/m, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (mls_u8_x_tied1, svuint8_t,
		z0 = svmls_u8_x (p0, z0, z1, z2),
		z0 = svmls_x (p0, z0, z1, z2))

/*
** mls_u8_x_tied2:
**	msb	z0\.b, p0/m, z2\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (mls_u8_x_tied2, svuint8_t,
		z0 = svmls_u8_x (p0, z1, z0, z2),
		z0 = svmls_x (p0, z1, z0, z2))

/*
** mls_u8_x_tied3:
**	msb	z0\.b, p0/m, z2\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (mls_u8_x_tied3, svuint8_t,
		z0 = svmls_u8_x (p0, z1, z2, z0),
		z0 = svmls_x (p0, z1, z2, z0))

/*
** mls_u8_x_untied:
** (
**	movprfx	z0, z1
**	mls	z0\.b, p0/m, z2\.b, z3\.b
** |
**	movprfx	z0, z2
**	msb	z0\.b, p0/m, z3\.b, z1\.b
** |
**	movprfx	z0, z3
**	msb	z0\.b, p0/m, z2\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (mls_u8_x_untied, svuint8_t,
		z0 = svmls_u8_x (p0, z1, z2, z3),
		z0 = svmls_x (p0, z1, z2, z3))

/*
** mls_w0_u8_x_tied1:
**	mov	(z[0-9]+\.b), w0
**	mls	z0\.b, p0/m, z1\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (mls_w0_u8_x_tied1, svuint8_t, uint8_t,
		 z0 = svmls_n_u8_x (p0, z0, z1, x0),
		 z0 = svmls_x (p0, z0, z1, x0))

/*
** mls_w0_u8_x_tied2:
**	mov	(z[0-9]+\.b), w0
**	msb	z0\.b, p0/m, \1, z1\.b
**	ret
*/
TEST_UNIFORM_ZX (mls_w0_u8_x_tied2, svuint8_t, uint8_t,
		 z0 = svmls_n_u8_x (p0, z1, z0, x0),
		 z0 = svmls_x (p0, z1, z0, x0))

/*
** mls_w0_u8_x_untied:
**	mov	z0\.b, w0
**	msb	z0\.b, p0/m, z2\.b, z1\.b
**	ret
*/
TEST_UNIFORM_ZX (mls_w0_u8_x_untied, svuint8_t, uint8_t,
		 z0 = svmls_n_u8_x (p0, z1, z2, x0),
		 z0 = svmls_x (p0, z1, z2, x0))

/*
** mls_11_u8_x_tied1:
**	mov	(z[0-9]+\.b), #11
**	mls	z0\.b, p0/m, z1\.b, \1
**	ret
*/
TEST_UNIFORM_Z (mls_11_u8_x_tied1, svuint8_t,
		z0 = svmls_n_u8_x (p0, z0, z1, 11),
		z0 = svmls_x (p0, z0, z1, 11))

/*
** mls_11_u8_x_tied2:
**	mov	(z[0-9]+\.b), #11
**	msb	z0\.b, p0/m, \1, z1\.b
**	ret
*/
TEST_UNIFORM_Z (mls_11_u8_x_tied2, svuint8_t,
		z0 = svmls_n_u8_x (p0, z1, z0, 11),
		z0 = svmls_x (p0, z1, z0, 11))

/*
** mls_11_u8_x_untied:
**	mov	z0\.b, #11
**	msb	z0\.b, p0/m, z2\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (mls_11_u8_x_untied, svuint8_t,
		z0 = svmls_n_u8_x (p0, z1, z2, 11),
		z0 = svmls_x (p0, z1, z2, 11))
