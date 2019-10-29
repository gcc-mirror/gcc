/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** add_s16_m_tied1:
**	add	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (add_s16_m_tied1, svint16_t,
		z0 = svadd_s16_m (p0, z0, z1),
		z0 = svadd_m (p0, z0, z1))

/*
** add_s16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	add	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (add_s16_m_tied2, svint16_t,
		z0 = svadd_s16_m (p0, z1, z0),
		z0 = svadd_m (p0, z1, z0))

/*
** add_s16_m_untied:
**	movprfx	z0, z1
**	add	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (add_s16_m_untied, svint16_t,
		z0 = svadd_s16_m (p0, z1, z2),
		z0 = svadd_m (p0, z1, z2))

/*
** add_w0_s16_m_tied1:
**	mov	(z[0-9]+\.h), w0
**	add	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (add_w0_s16_m_tied1, svint16_t, int16_t,
		 z0 = svadd_n_s16_m (p0, z0, x0),
		 z0 = svadd_m (p0, z0, x0))

/*
** add_w0_s16_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	add	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (add_w0_s16_m_untied, svint16_t, int16_t,
		 z0 = svadd_n_s16_m (p0, z1, x0),
		 z0 = svadd_m (p0, z1, x0))

/*
** add_1_s16_m_tied1:
**	mov	(z[0-9]+\.h), #1
**	add	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (add_1_s16_m_tied1, svint16_t,
		z0 = svadd_n_s16_m (p0, z0, 1),
		z0 = svadd_m (p0, z0, 1))

/*
** add_1_s16_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.h), #1
**	movprfx	z0, z1
**	add	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (add_1_s16_m_untied, svint16_t,
		z0 = svadd_n_s16_m (p0, z1, 1),
		z0 = svadd_m (p0, z1, 1))

/*
** add_m2_s16_m:
**	mov	(z[0-9]+\.h), #-2
**	add	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (add_m2_s16_m, svint16_t,
		z0 = svadd_n_s16_m (p0, z0, -2),
		z0 = svadd_m (p0, z0, -2))

/*
** add_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	add	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (add_s16_z_tied1, svint16_t,
		z0 = svadd_s16_z (p0, z0, z1),
		z0 = svadd_z (p0, z0, z1))

/*
** add_s16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	add	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (add_s16_z_tied2, svint16_t,
		z0 = svadd_s16_z (p0, z1, z0),
		z0 = svadd_z (p0, z1, z0))

/*
** add_s16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	add	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0\.h, p0/z, z2\.h
**	add	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (add_s16_z_untied, svint16_t,
		z0 = svadd_s16_z (p0, z1, z2),
		z0 = svadd_z (p0, z1, z2))

/*
** add_w0_s16_z_tied1:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	add	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (add_w0_s16_z_tied1, svint16_t, int16_t,
		 z0 = svadd_n_s16_z (p0, z0, x0),
		 z0 = svadd_z (p0, z0, x0))

/*
** add_w0_s16_z_untied:
**	mov	(z[0-9]+\.h), w0
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	add	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	add	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZX (add_w0_s16_z_untied, svint16_t, int16_t,
		 z0 = svadd_n_s16_z (p0, z1, x0),
		 z0 = svadd_z (p0, z1, x0))

/*
** add_1_s16_z_tied1:
**	mov	(z[0-9]+\.h), #1
**	movprfx	z0\.h, p0/z, z0\.h
**	add	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (add_1_s16_z_tied1, svint16_t,
		z0 = svadd_n_s16_z (p0, z0, 1),
		z0 = svadd_z (p0, z0, 1))

/*
** add_1_s16_z_untied:
**	mov	(z[0-9]+\.h), #1
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	add	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	add	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (add_1_s16_z_untied, svint16_t,
		z0 = svadd_n_s16_z (p0, z1, 1),
		z0 = svadd_z (p0, z1, 1))

/*
** add_s16_x_tied1:
**	add	z0\.h, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_UNIFORM_Z (add_s16_x_tied1, svint16_t,
		z0 = svadd_s16_x (p0, z0, z1),
		z0 = svadd_x (p0, z0, z1))

/*
** add_s16_x_tied2:
**	add	z0\.h, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_UNIFORM_Z (add_s16_x_tied2, svint16_t,
		z0 = svadd_s16_x (p0, z1, z0),
		z0 = svadd_x (p0, z1, z0))

/*
** add_s16_x_untied:
**	add	z0\.h, (z1\.h, z2\.h|z2\.h, z1\.h)
**	ret
*/
TEST_UNIFORM_Z (add_s16_x_untied, svint16_t,
		z0 = svadd_s16_x (p0, z1, z2),
		z0 = svadd_x (p0, z1, z2))

/*
** add_w0_s16_x_tied1:
**	mov	(z[0-9]+\.h), w0
**	add	z0\.h, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_UNIFORM_ZX (add_w0_s16_x_tied1, svint16_t, int16_t,
		 z0 = svadd_n_s16_x (p0, z0, x0),
		 z0 = svadd_x (p0, z0, x0))

/*
** add_w0_s16_x_untied:
**	mov	(z[0-9]+\.h), w0
**	add	z0\.h, (z1\.h, \1|\1, z1\.h)
**	ret
*/
TEST_UNIFORM_ZX (add_w0_s16_x_untied, svint16_t, int16_t,
		 z0 = svadd_n_s16_x (p0, z1, x0),
		 z0 = svadd_x (p0, z1, x0))

/*
** add_1_s16_x_tied1:
**	add	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (add_1_s16_x_tied1, svint16_t,
		z0 = svadd_n_s16_x (p0, z0, 1),
		z0 = svadd_x (p0, z0, 1))

/*
** add_1_s16_x_untied:
**	movprfx	z0, z1
**	add	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (add_1_s16_x_untied, svint16_t,
		z0 = svadd_n_s16_x (p0, z1, 1),
		z0 = svadd_x (p0, z1, 1))

/*
** add_127_s16_x:
**	add	z0\.h, z0\.h, #127
**	ret
*/
TEST_UNIFORM_Z (add_127_s16_x, svint16_t,
		z0 = svadd_n_s16_x (p0, z0, 127),
		z0 = svadd_x (p0, z0, 127))

/*
** add_128_s16_x:
**	add	z0\.h, z0\.h, #128
**	ret
*/
TEST_UNIFORM_Z (add_128_s16_x, svint16_t,
		z0 = svadd_n_s16_x (p0, z0, 128),
		z0 = svadd_x (p0, z0, 128))

/*
** add_255_s16_x:
**	add	z0\.h, z0\.h, #255
**	ret
*/
TEST_UNIFORM_Z (add_255_s16_x, svint16_t,
		z0 = svadd_n_s16_x (p0, z0, 255),
		z0 = svadd_x (p0, z0, 255))

/*
** add_256_s16_x:
**	add	z0\.h, z0\.h, #256
**	ret
*/
TEST_UNIFORM_Z (add_256_s16_x, svint16_t,
		z0 = svadd_n_s16_x (p0, z0, 256),
		z0 = svadd_x (p0, z0, 256))

/*
** add_257_s16_x:
**	mov	(z[0-9]+)\.b, #1
**	add	z0\.h, (z0\.h, \1\.h|\1\.h, z0\.h)
**	ret
*/
TEST_UNIFORM_Z (add_257_s16_x, svint16_t,
		z0 = svadd_n_s16_x (p0, z0, 257),
		z0 = svadd_x (p0, z0, 257))

/*
** add_512_s16_x:
**	add	z0\.h, z0\.h, #512
**	ret
*/
TEST_UNIFORM_Z (add_512_s16_x, svint16_t,
		z0 = svadd_n_s16_x (p0, z0, 512),
		z0 = svadd_x (p0, z0, 512))

/*
** add_65280_s16_x:
**	add	z0\.h, z0\.h, #65280
**	ret
*/
TEST_UNIFORM_Z (add_65280_s16_x, svint16_t,
		z0 = svadd_n_s16_x (p0, z0, 0xff00),
		z0 = svadd_x (p0, z0, 0xff00))

/*
** add_m1_s16_x:
**	sub	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (add_m1_s16_x, svint16_t,
		z0 = svadd_n_s16_x (p0, z0, -1),
		z0 = svadd_x (p0, z0, -1))

/*
** add_m127_s16_x:
**	sub	z0\.h, z0\.h, #127
**	ret
*/
TEST_UNIFORM_Z (add_m127_s16_x, svint16_t,
		z0 = svadd_n_s16_x (p0, z0, -127),
		z0 = svadd_x (p0, z0, -127))

/*
** add_m128_s16_x:
**	sub	z0\.h, z0\.h, #128
**	ret
*/
TEST_UNIFORM_Z (add_m128_s16_x, svint16_t,
		z0 = svadd_n_s16_x (p0, z0, -128),
		z0 = svadd_x (p0, z0, -128))

/*
** add_m255_s16_x:
**	sub	z0\.h, z0\.h, #255
**	ret
*/
TEST_UNIFORM_Z (add_m255_s16_x, svint16_t,
		z0 = svadd_n_s16_x (p0, z0, -255),
		z0 = svadd_x (p0, z0, -255))

/*
** add_m256_s16_x:
**	add	z0\.h, z0\.h, #65280
**	ret
*/
TEST_UNIFORM_Z (add_m256_s16_x, svint16_t,
		z0 = svadd_n_s16_x (p0, z0, -256),
		z0 = svadd_x (p0, z0, -256))

/*
** add_m257_s16_x:
**	mov	(z[0-9]+\.h), #-257
**	add	z0\.h, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_UNIFORM_Z (add_m257_s16_x, svint16_t,
		z0 = svadd_n_s16_x (p0, z0, -257),
		z0 = svadd_x (p0, z0, -257))

/*
** add_m512_s16_x:
**	add	z0\.h, z0\.h, #65024
**	ret
*/
TEST_UNIFORM_Z (add_m512_s16_x, svint16_t,
		z0 = svadd_n_s16_x (p0, z0, -512),
		z0 = svadd_x (p0, z0, -512))

/*
** add_m32768_s16_x:
**	add	z0\.h, z0\.h, #32768
**	ret
*/
TEST_UNIFORM_Z (add_m32768_s16_x, svint16_t,
		z0 = svadd_n_s16_x (p0, z0, -0x8000),
		z0 = svadd_x (p0, z0, -0x8000))
