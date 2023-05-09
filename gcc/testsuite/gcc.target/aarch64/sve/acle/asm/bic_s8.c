/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** bic_s8_m_tied1:
**	bic	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (bic_s8_m_tied1, svint8_t,
		z0 = svbic_s8_m (p0, z0, z1),
		z0 = svbic_m (p0, z0, z1))

/*
** bic_s8_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	bic	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_UNIFORM_Z (bic_s8_m_tied2, svint8_t,
		z0 = svbic_s8_m (p0, z1, z0),
		z0 = svbic_m (p0, z1, z0))

/*
** bic_s8_m_untied:
**	movprfx	z0, z1
**	bic	z0\.b, p0/m, z0\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (bic_s8_m_untied, svint8_t,
		z0 = svbic_s8_m (p0, z1, z2),
		z0 = svbic_m (p0, z1, z2))

/*
** bic_w0_s8_m_tied1:
**	mov	(z[0-9]+\.b), w0
**	bic	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (bic_w0_s8_m_tied1, svint8_t, int8_t,
		 z0 = svbic_n_s8_m (p0, z0, x0),
		 z0 = svbic_m (p0, z0, x0))

/*
** bic_w0_s8_m_untied:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	bic	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (bic_w0_s8_m_untied, svint8_t, int8_t,
		 z0 = svbic_n_s8_m (p0, z1, x0),
		 z0 = svbic_m (p0, z1, x0))

/*
** bic_1_s8_m_tied1:
**	mov	(z[0-9]+\.b), #-2
**	and	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (bic_1_s8_m_tied1, svint8_t,
		z0 = svbic_n_s8_m (p0, z0, 1),
		z0 = svbic_m (p0, z0, 1))

/*
** bic_1_s8_m_untied:
**	mov	(z[0-9]+\.b), #-2
**	movprfx	z0, z1
**	and	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (bic_1_s8_m_untied, svint8_t,
		z0 = svbic_n_s8_m (p0, z1, 1),
		z0 = svbic_m (p0, z1, 1))

/*
** bic_m2_s8_m:
**	mov	(z[0-9]+\.b), #1
**	and	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (bic_m2_s8_m, svint8_t,
		z0 = svbic_n_s8_m (p0, z0, -2),
		z0 = svbic_m (p0, z0, -2))

/*
** bic_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	bic	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (bic_s8_z_tied1, svint8_t,
		z0 = svbic_s8_z (p0, z0, z1),
		z0 = svbic_z (p0, z0, z1))

/*
** bic_s8_z_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.b, p0/z, z1\.b
**	bic	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_UNIFORM_Z (bic_s8_z_tied2, svint8_t,
		z0 = svbic_s8_z (p0, z1, z0),
		z0 = svbic_z (p0, z1, z0))

/*
** bic_s8_z_untied:
**	movprfx	z0\.b, p0/z, z1\.b
**	bic	z0\.b, p0/m, z0\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (bic_s8_z_untied, svint8_t,
		z0 = svbic_s8_z (p0, z1, z2),
		z0 = svbic_z (p0, z1, z2))

/*
** bic_w0_s8_z_tied1:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0\.b, p0/z, z0\.b
**	bic	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (bic_w0_s8_z_tied1, svint8_t, int8_t,
		 z0 = svbic_n_s8_z (p0, z0, x0),
		 z0 = svbic_z (p0, z0, x0))

/*
** bic_w0_s8_z_untied:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0\.b, p0/z, z1\.b
**	bic	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (bic_w0_s8_z_untied, svint8_t, int8_t,
		 z0 = svbic_n_s8_z (p0, z1, x0),
		 z0 = svbic_z (p0, z1, x0))

/*
** bic_1_s8_z_tied1:
**	mov	(z[0-9]+\.b), #-2
**	movprfx	z0\.b, p0/z, z0\.b
**	and	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (bic_1_s8_z_tied1, svint8_t,
		z0 = svbic_n_s8_z (p0, z0, 1),
		z0 = svbic_z (p0, z0, 1))

/*
** bic_1_s8_z_untied:
**	mov	(z[0-9]+\.b), #-2
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	and	z0\.b, p0/m, z0\.b, \1
** |
**	movprfx	z0\.b, p0/z, \1
**	and	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (bic_1_s8_z_untied, svint8_t,
		z0 = svbic_n_s8_z (p0, z1, 1),
		z0 = svbic_z (p0, z1, 1))

/*
** bic_s8_x_tied1:
**	bic	z0\.d, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (bic_s8_x_tied1, svint8_t,
		z0 = svbic_s8_x (p0, z0, z1),
		z0 = svbic_x (p0, z0, z1))

/*
** bic_s8_x_tied2:
**	bic	z0\.d, z1\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (bic_s8_x_tied2, svint8_t,
		z0 = svbic_s8_x (p0, z1, z0),
		z0 = svbic_x (p0, z1, z0))

/*
** bic_s8_x_untied:
**	bic	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (bic_s8_x_untied, svint8_t,
		z0 = svbic_s8_x (p0, z1, z2),
		z0 = svbic_x (p0, z1, z2))

/*
** bic_w0_s8_x_tied1:
**	mov	(z[0-9]+)\.b, w0
**	bic	z0\.d, z0\.d, \1\.d
**	ret
*/
TEST_UNIFORM_ZX (bic_w0_s8_x_tied1, svint8_t, int8_t,
		 z0 = svbic_n_s8_x (p0, z0, x0),
		 z0 = svbic_x (p0, z0, x0))

/*
** bic_w0_s8_x_untied:
**	mov	(z[0-9]+)\.b, w0
**	bic	z0\.d, z1\.d, \1\.d
**	ret
*/
TEST_UNIFORM_ZX (bic_w0_s8_x_untied, svint8_t, int8_t,
		 z0 = svbic_n_s8_x (p0, z1, x0),
		 z0 = svbic_x (p0, z1, x0))

/*
** bic_1_s8_x_tied1:
**	and	z0\.b, z0\.b, #0xfe
**	ret
*/
TEST_UNIFORM_Z (bic_1_s8_x_tied1, svint8_t,
		z0 = svbic_n_s8_x (p0, z0, 1),
		z0 = svbic_x (p0, z0, 1))

/*
** bic_1_s8_x_untied:
**	movprfx	z0, z1
**	and	z0\.b, z0\.b, #0xfe
**	ret
*/
TEST_UNIFORM_Z (bic_1_s8_x_untied, svint8_t,
		z0 = svbic_n_s8_x (p0, z1, 1),
		z0 = svbic_x (p0, z1, 1))

/*
** bic_127_s8_x:
**	and	z0\.b, z0\.b, #0x80
**	ret
*/
TEST_UNIFORM_Z (bic_127_s8_x, svint8_t,
		z0 = svbic_n_s8_x (p0, z0, 127),
		z0 = svbic_x (p0, z0, 127))

/*
** bic_128_s8_x:
**	and	z0\.b, z0\.b, #0x7f
**	ret
*/
TEST_UNIFORM_Z (bic_128_s8_x, svint8_t,
		z0 = svbic_n_s8_x (p0, z0, 128),
		z0 = svbic_x (p0, z0, 128))

/*
** bic_255_s8_x:
**	mov	z0\.b, #0
**	ret
*/
TEST_UNIFORM_Z (bic_255_s8_x, svint8_t,
		z0 = svbic_n_s8_x (p0, z0, 255),
		z0 = svbic_x (p0, z0, 255))

/*
** bic_m127_s8_x:
**	and	z0\.b, z0\.b, #0x7e
**	ret
*/
TEST_UNIFORM_Z (bic_m127_s8_x, svint8_t,
		z0 = svbic_n_s8_x (p0, z0, -127),
		z0 = svbic_x (p0, z0, -127))

/*
** bic_m128_s8_x:
**	and	z0\.b, z0\.b, #0x7f
**	ret
*/
TEST_UNIFORM_Z (bic_m128_s8_x, svint8_t,
		z0 = svbic_n_s8_x (p0, z0, -128),
		z0 = svbic_x (p0, z0, -128))

/*
** bic_5_s8_x:
**	mov	(z[0-9]+)\.b, #-6
**	and	z0\.d, (z0\.d, \1\.d|\1\.d, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (bic_5_s8_x, svint8_t,
		z0 = svbic_n_s8_x (p0, z0, 5),
		z0 = svbic_x (p0, z0, 5))
