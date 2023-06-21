/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** eor_u32_m_tied1:
**	eor	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (eor_u32_m_tied1, svuint32_t,
		z0 = sveor_u32_m (p0, z0, z1),
		z0 = sveor_m (p0, z0, z1))

/*
** eor_u32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	eor	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (eor_u32_m_tied2, svuint32_t,
		z0 = sveor_u32_m (p0, z1, z0),
		z0 = sveor_m (p0, z1, z0))

/*
** eor_u32_m_untied:
**	movprfx	z0, z1
**	eor	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (eor_u32_m_untied, svuint32_t,
		z0 = sveor_u32_m (p0, z1, z2),
		z0 = sveor_m (p0, z1, z2))

/*
** eor_w0_u32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	eor	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (eor_w0_u32_m_tied1, svuint32_t, uint32_t,
		 z0 = sveor_n_u32_m (p0, z0, x0),
		 z0 = sveor_m (p0, z0, x0))

/*
** eor_w0_u32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	eor	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (eor_w0_u32_m_untied, svuint32_t, uint32_t,
		 z0 = sveor_n_u32_m (p0, z1, x0),
		 z0 = sveor_m (p0, z1, x0))

/*
** eor_1_u32_m_tied1:
**	mov	(z[0-9]+\.s), #1
**	eor	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (eor_1_u32_m_tied1, svuint32_t,
		z0 = sveor_n_u32_m (p0, z0, 1),
		z0 = sveor_m (p0, z0, 1))

/*
** eor_1_u32_m_untied:
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0, z1
**	eor	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (eor_1_u32_m_untied, svuint32_t,
		z0 = sveor_n_u32_m (p0, z1, 1),
		z0 = sveor_m (p0, z1, 1))

/*
** eor_m2_u32_m:
**	mov	(z[0-9]+\.s), #-2
**	eor	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (eor_m2_u32_m, svuint32_t,
		z0 = sveor_n_u32_m (p0, z0, -2),
		z0 = sveor_m (p0, z0, -2))

/*
** eor_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	eor	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (eor_u32_z_tied1, svuint32_t,
		z0 = sveor_u32_z (p0, z0, z1),
		z0 = sveor_z (p0, z0, z1))

/*
** eor_u32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	eor	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (eor_u32_z_tied2, svuint32_t,
		z0 = sveor_u32_z (p0, z1, z0),
		z0 = sveor_z (p0, z1, z0))

/*
** eor_u32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	eor	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	eor	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (eor_u32_z_untied, svuint32_t,
		z0 = sveor_u32_z (p0, z1, z2),
		z0 = sveor_z (p0, z1, z2))

/*
** eor_w0_u32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	eor	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (eor_w0_u32_z_tied1, svuint32_t, uint32_t,
		 z0 = sveor_n_u32_z (p0, z0, x0),
		 z0 = sveor_z (p0, z0, x0))

/*
** eor_w0_u32_z_untied:
**	mov	(z[0-9]+\.s), w0
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	eor	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	eor	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZX (eor_w0_u32_z_untied, svuint32_t, uint32_t,
		 z0 = sveor_n_u32_z (p0, z1, x0),
		 z0 = sveor_z (p0, z1, x0))

/*
** eor_1_u32_z_tied1:
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0\.s, p0/z, z0\.s
**	eor	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (eor_1_u32_z_tied1, svuint32_t,
		z0 = sveor_n_u32_z (p0, z0, 1),
		z0 = sveor_z (p0, z0, 1))

/*
** eor_1_u32_z_untied:
**	mov	(z[0-9]+\.s), #1
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	eor	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	eor	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (eor_1_u32_z_untied, svuint32_t,
		z0 = sveor_n_u32_z (p0, z1, 1),
		z0 = sveor_z (p0, z1, 1))

/*
** eor_u32_x_tied1:
**	eor	z0\.d, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (eor_u32_x_tied1, svuint32_t,
		z0 = sveor_u32_x (p0, z0, z1),
		z0 = sveor_x (p0, z0, z1))

/*
** eor_u32_x_tied2:
**	eor	z0\.d, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (eor_u32_x_tied2, svuint32_t,
		z0 = sveor_u32_x (p0, z1, z0),
		z0 = sveor_x (p0, z1, z0))

/*
** eor_u32_x_untied:
**	eor	z0\.d, (z1\.d, z2\.d|z2\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_Z (eor_u32_x_untied, svuint32_t,
		z0 = sveor_u32_x (p0, z1, z2),
		z0 = sveor_x (p0, z1, z2))

/*
** eor_w0_u32_x_tied1:
**	mov	(z[0-9]+)\.s, w0
**	eor	z0\.d, (z0\.d, \1\.d|\1\.d, z0\.d)
**	ret
*/
TEST_UNIFORM_ZX (eor_w0_u32_x_tied1, svuint32_t, uint32_t,
		 z0 = sveor_n_u32_x (p0, z0, x0),
		 z0 = sveor_x (p0, z0, x0))

/*
** eor_w0_u32_x_untied:
**	mov	(z[0-9]+)\.s, w0
**	eor	z0\.d, (z1\.d, \1\.d|\1\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_ZX (eor_w0_u32_x_untied, svuint32_t, uint32_t,
		 z0 = sveor_n_u32_x (p0, z1, x0),
		 z0 = sveor_x (p0, z1, x0))

/*
** eor_1_u32_x_tied1:
**	eor	z0\.s, z0\.s, #0x1
**	ret
*/
TEST_UNIFORM_Z (eor_1_u32_x_tied1, svuint32_t,
		z0 = sveor_n_u32_x (p0, z0, 1),
		z0 = sveor_x (p0, z0, 1))

/*
** eor_1_u32_x_untied:
**	movprfx	z0, z1
**	eor	z0\.s, z0\.s, #0x1
**	ret
*/
TEST_UNIFORM_Z (eor_1_u32_x_untied, svuint32_t,
		z0 = sveor_n_u32_x (p0, z1, 1),
		z0 = sveor_x (p0, z1, 1))

/*
** eor_127_u32_x:
**	eor	z0\.s, z0\.s, #0x7f
**	ret
*/
TEST_UNIFORM_Z (eor_127_u32_x, svuint32_t,
		z0 = sveor_n_u32_x (p0, z0, 127),
		z0 = sveor_x (p0, z0, 127))

/*
** eor_128_u32_x:
**	eor	z0\.s, z0\.s, #0x80
**	ret
*/
TEST_UNIFORM_Z (eor_128_u32_x, svuint32_t,
		z0 = sveor_n_u32_x (p0, z0, 128),
		z0 = sveor_x (p0, z0, 128))

/*
** eor_255_u32_x:
**	eor	z0\.s, z0\.s, #0xff
**	ret
*/
TEST_UNIFORM_Z (eor_255_u32_x, svuint32_t,
		z0 = sveor_n_u32_x (p0, z0, 255),
		z0 = sveor_x (p0, z0, 255))

/*
** eor_256_u32_x:
**	eor	z0\.s, z0\.s, #0x100
**	ret
*/
TEST_UNIFORM_Z (eor_256_u32_x, svuint32_t,
		z0 = sveor_n_u32_x (p0, z0, 256),
		z0 = sveor_x (p0, z0, 256))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (eor_257_u32_x, svuint32_t,
		z0 = sveor_n_u32_x (p0, z0, 257),
		z0 = sveor_x (p0, z0, 257))

/*
** eor_512_u32_x:
**	eor	z0\.s, z0\.s, #0x200
**	ret
*/
TEST_UNIFORM_Z (eor_512_u32_x, svuint32_t,
		z0 = sveor_n_u32_x (p0, z0, 512),
		z0 = sveor_x (p0, z0, 512))

/*
** eor_65280_u32_x:
**	eor	z0\.s, z0\.s, #0xff00
**	ret
*/
TEST_UNIFORM_Z (eor_65280_u32_x, svuint32_t,
		z0 = sveor_n_u32_x (p0, z0, 0xff00),
		z0 = sveor_x (p0, z0, 0xff00))

/*
** eor_m127_u32_x:
**	eor	z0\.s, z0\.s, #0xffffff81
**	ret
*/
TEST_UNIFORM_Z (eor_m127_u32_x, svuint32_t,
		z0 = sveor_n_u32_x (p0, z0, -127),
		z0 = sveor_x (p0, z0, -127))

/*
** eor_m128_u32_x:
**	eor	z0\.s, z0\.s, #0xffffff80
**	ret
*/
TEST_UNIFORM_Z (eor_m128_u32_x, svuint32_t,
		z0 = sveor_n_u32_x (p0, z0, -128),
		z0 = sveor_x (p0, z0, -128))

/*
** eor_m255_u32_x:
**	eor	z0\.s, z0\.s, #0xffffff01
**	ret
*/
TEST_UNIFORM_Z (eor_m255_u32_x, svuint32_t,
		z0 = sveor_n_u32_x (p0, z0, -255),
		z0 = sveor_x (p0, z0, -255))

/*
** eor_m256_u32_x:
**	eor	z0\.s, z0\.s, #0xffffff00
**	ret
*/
TEST_UNIFORM_Z (eor_m256_u32_x, svuint32_t,
		z0 = sveor_n_u32_x (p0, z0, -256),
		z0 = sveor_x (p0, z0, -256))

/*
** eor_m257_u32_x:
**	eor	z0\.s, z0\.s, #0xfffffeff
**	ret
*/
TEST_UNIFORM_Z (eor_m257_u32_x, svuint32_t,
		z0 = sveor_n_u32_x (p0, z0, -257),
		z0 = sveor_x (p0, z0, -257))

/*
** eor_m512_u32_x:
**	eor	z0\.s, z0\.s, #0xfffffe00
**	ret
*/
TEST_UNIFORM_Z (eor_m512_u32_x, svuint32_t,
		z0 = sveor_n_u32_x (p0, z0, -512),
		z0 = sveor_x (p0, z0, -512))

/*
** eor_m32768_u32_x:
**	eor	z0\.s, z0\.s, #0xffff8000
**	ret
*/
TEST_UNIFORM_Z (eor_m32768_u32_x, svuint32_t,
		z0 = sveor_n_u32_x (p0, z0, -0x8000),
		z0 = sveor_x (p0, z0, -0x8000))

/*
** eor_5_u32_x:
**	mov	(z[0-9]+)\.s, #5
**	eor	z0\.d, (z0\.d, \1\.d|\1\.d, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (eor_5_u32_x, svuint32_t,
		z0 = sveor_n_u32_x (p0, z0, 5),
		z0 = sveor_x (p0, z0, 5))
