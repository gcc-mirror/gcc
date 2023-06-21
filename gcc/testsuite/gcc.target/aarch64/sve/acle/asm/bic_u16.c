/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** bic_u16_m_tied1:
**	bic	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (bic_u16_m_tied1, svuint16_t,
		z0 = svbic_u16_m (p0, z0, z1),
		z0 = svbic_m (p0, z0, z1))

/*
** bic_u16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	bic	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (bic_u16_m_tied2, svuint16_t,
		z0 = svbic_u16_m (p0, z1, z0),
		z0 = svbic_m (p0, z1, z0))

/*
** bic_u16_m_untied:
**	movprfx	z0, z1
**	bic	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (bic_u16_m_untied, svuint16_t,
		z0 = svbic_u16_m (p0, z1, z2),
		z0 = svbic_m (p0, z1, z2))

/*
** bic_w0_u16_m_tied1:
**	mov	(z[0-9]+\.h), w0
**	bic	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (bic_w0_u16_m_tied1, svuint16_t, uint16_t,
		 z0 = svbic_n_u16_m (p0, z0, x0),
		 z0 = svbic_m (p0, z0, x0))

/*
** bic_w0_u16_m_untied:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	bic	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (bic_w0_u16_m_untied, svuint16_t, uint16_t,
		 z0 = svbic_n_u16_m (p0, z1, x0),
		 z0 = svbic_m (p0, z1, x0))

/*
** bic_1_u16_m_tied1:
**	mov	(z[0-9]+\.h), #-2
**	and	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (bic_1_u16_m_tied1, svuint16_t,
		z0 = svbic_n_u16_m (p0, z0, 1),
		z0 = svbic_m (p0, z0, 1))

/*
** bic_1_u16_m_untied:
**	mov	(z[0-9]+\.h), #-2
**	movprfx	z0, z1
**	and	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (bic_1_u16_m_untied, svuint16_t,
		z0 = svbic_n_u16_m (p0, z1, 1),
		z0 = svbic_m (p0, z1, 1))

/*
** bic_m2_u16_m:
**	mov	(z[0-9]+\.h), #1
**	and	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (bic_m2_u16_m, svuint16_t,
		z0 = svbic_n_u16_m (p0, z0, -2),
		z0 = svbic_m (p0, z0, -2))

/*
** bic_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	bic	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (bic_u16_z_tied1, svuint16_t,
		z0 = svbic_u16_z (p0, z0, z1),
		z0 = svbic_z (p0, z0, z1))

/*
** bic_u16_z_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.h, p0/z, z1\.h
**	bic	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (bic_u16_z_tied2, svuint16_t,
		z0 = svbic_u16_z (p0, z1, z0),
		z0 = svbic_z (p0, z1, z0))

/*
** bic_u16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	bic	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (bic_u16_z_untied, svuint16_t,
		z0 = svbic_u16_z (p0, z1, z2),
		z0 = svbic_z (p0, z1, z2))

/*
** bic_w0_u16_z_tied1:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	bic	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (bic_w0_u16_z_tied1, svuint16_t, uint16_t,
		 z0 = svbic_n_u16_z (p0, z0, x0),
		 z0 = svbic_z (p0, z0, x0))

/*
** bic_w0_u16_z_untied:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z1\.h
**	bic	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (bic_w0_u16_z_untied, svuint16_t, uint16_t,
		 z0 = svbic_n_u16_z (p0, z1, x0),
		 z0 = svbic_z (p0, z1, x0))

/*
** bic_1_u16_z_tied1:
**	mov	(z[0-9]+\.h), #-2
**	movprfx	z0\.h, p0/z, z0\.h
**	and	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (bic_1_u16_z_tied1, svuint16_t,
		z0 = svbic_n_u16_z (p0, z0, 1),
		z0 = svbic_z (p0, z0, 1))

/*
** bic_1_u16_z_untied:
**	mov	(z[0-9]+\.h), #-2
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	and	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	and	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (bic_1_u16_z_untied, svuint16_t,
		z0 = svbic_n_u16_z (p0, z1, 1),
		z0 = svbic_z (p0, z1, 1))

/*
** bic_u16_x_tied1:
**	bic	z0\.d, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (bic_u16_x_tied1, svuint16_t,
		z0 = svbic_u16_x (p0, z0, z1),
		z0 = svbic_x (p0, z0, z1))

/*
** bic_u16_x_tied2:
**	bic	z0\.d, z1\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (bic_u16_x_tied2, svuint16_t,
		z0 = svbic_u16_x (p0, z1, z0),
		z0 = svbic_x (p0, z1, z0))

/*
** bic_u16_x_untied:
**	bic	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (bic_u16_x_untied, svuint16_t,
		z0 = svbic_u16_x (p0, z1, z2),
		z0 = svbic_x (p0, z1, z2))

/*
** bic_w0_u16_x_tied1:
**	mov	(z[0-9]+)\.h, w0
**	bic	z0\.d, z0\.d, \1\.d
**	ret
*/
TEST_UNIFORM_ZX (bic_w0_u16_x_tied1, svuint16_t, uint16_t,
		 z0 = svbic_n_u16_x (p0, z0, x0),
		 z0 = svbic_x (p0, z0, x0))

/*
** bic_w0_u16_x_untied:
**	mov	(z[0-9]+)\.h, w0
**	bic	z0\.d, z1\.d, \1\.d
**	ret
*/
TEST_UNIFORM_ZX (bic_w0_u16_x_untied, svuint16_t, uint16_t,
		 z0 = svbic_n_u16_x (p0, z1, x0),
		 z0 = svbic_x (p0, z1, x0))

/*
** bic_1_u16_x_tied1:
**	and	z0\.h, z0\.h, #0xfffe
**	ret
*/
TEST_UNIFORM_Z (bic_1_u16_x_tied1, svuint16_t,
		z0 = svbic_n_u16_x (p0, z0, 1),
		z0 = svbic_x (p0, z0, 1))

/*
** bic_1_u16_x_untied:
**	movprfx	z0, z1
**	and	z0\.h, z0\.h, #0xfffe
**	ret
*/
TEST_UNIFORM_Z (bic_1_u16_x_untied, svuint16_t,
		z0 = svbic_n_u16_x (p0, z1, 1),
		z0 = svbic_x (p0, z1, 1))

/*
** bic_127_u16_x:
**	and	z0\.h, z0\.h, #0xff80
**	ret
*/
TEST_UNIFORM_Z (bic_127_u16_x, svuint16_t,
		z0 = svbic_n_u16_x (p0, z0, 127),
		z0 = svbic_x (p0, z0, 127))

/*
** bic_128_u16_x:
**	and	z0\.h, z0\.h, #0xff7f
**	ret
*/
TEST_UNIFORM_Z (bic_128_u16_x, svuint16_t,
		z0 = svbic_n_u16_x (p0, z0, 128),
		z0 = svbic_x (p0, z0, 128))

/*
** bic_255_u16_x:
**	and	z0\.h, z0\.h, #0xff00
**	ret
*/
TEST_UNIFORM_Z (bic_255_u16_x, svuint16_t,
		z0 = svbic_n_u16_x (p0, z0, 255),
		z0 = svbic_x (p0, z0, 255))

/*
** bic_256_u16_x:
**	and	z0\.h, z0\.h, #0xfeff
**	ret
*/
TEST_UNIFORM_Z (bic_256_u16_x, svuint16_t,
		z0 = svbic_n_u16_x (p0, z0, 256),
		z0 = svbic_x (p0, z0, 256))

/*
** bic_257_u16_x:
**	and	z0\.h, z0\.h, #0xfefe
**	ret
*/
TEST_UNIFORM_Z (bic_257_u16_x, svuint16_t,
		z0 = svbic_n_u16_x (p0, z0, 257),
		z0 = svbic_x (p0, z0, 257))

/*
** bic_512_u16_x:
**	and	z0\.h, z0\.h, #0xfdff
**	ret
*/
TEST_UNIFORM_Z (bic_512_u16_x, svuint16_t,
		z0 = svbic_n_u16_x (p0, z0, 512),
		z0 = svbic_x (p0, z0, 512))

/*
** bic_65280_u16_x:
**	and	z0\.h, z0\.h, #0xff
**	ret
*/
TEST_UNIFORM_Z (bic_65280_u16_x, svuint16_t,
		z0 = svbic_n_u16_x (p0, z0, 0xff00),
		z0 = svbic_x (p0, z0, 0xff00))

/*
** bic_m127_u16_x:
**	and	z0\.h, z0\.h, #0x7e
**	ret
*/
TEST_UNIFORM_Z (bic_m127_u16_x, svuint16_t,
		z0 = svbic_n_u16_x (p0, z0, -127),
		z0 = svbic_x (p0, z0, -127))

/*
** bic_m128_u16_x:
**	and	z0\.h, z0\.h, #0x7f
**	ret
*/
TEST_UNIFORM_Z (bic_m128_u16_x, svuint16_t,
		z0 = svbic_n_u16_x (p0, z0, -128),
		z0 = svbic_x (p0, z0, -128))

/*
** bic_m255_u16_x:
**	and	z0\.h, z0\.h, #0xfe
**	ret
*/
TEST_UNIFORM_Z (bic_m255_u16_x, svuint16_t,
		z0 = svbic_n_u16_x (p0, z0, -255),
		z0 = svbic_x (p0, z0, -255))

/*
** bic_m256_u16_x:
**	and	z0\.h, z0\.h, #0xff
**	ret
*/
TEST_UNIFORM_Z (bic_m256_u16_x, svuint16_t,
		z0 = svbic_n_u16_x (p0, z0, -256),
		z0 = svbic_x (p0, z0, -256))

/*
** bic_m257_u16_x:
**	and	z0\.h, z0\.h, #0x100
**	ret
*/
TEST_UNIFORM_Z (bic_m257_u16_x, svuint16_t,
		z0 = svbic_n_u16_x (p0, z0, -257),
		z0 = svbic_x (p0, z0, -257))

/*
** bic_m512_u16_x:
**	and	z0\.h, z0\.h, #0x1ff
**	ret
*/
TEST_UNIFORM_Z (bic_m512_u16_x, svuint16_t,
		z0 = svbic_n_u16_x (p0, z0, -512),
		z0 = svbic_x (p0, z0, -512))

/*
** bic_m32768_u16_x:
**	and	z0\.h, z0\.h, #0x7fff
**	ret
*/
TEST_UNIFORM_Z (bic_m32768_u16_x, svuint16_t,
		z0 = svbic_n_u16_x (p0, z0, -0x8000),
		z0 = svbic_x (p0, z0, -0x8000))

/*
** bic_5_u16_x:
**	mov	(z[0-9]+)\.h, #-6
**	and	z0\.d, (z0\.d, \1\.d|\1\.d, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (bic_5_u16_x, svuint16_t,
		z0 = svbic_n_u16_x (p0, z0, 5),
		z0 = svbic_x (p0, z0, 5))
