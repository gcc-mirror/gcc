/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** orr_s64_m_tied1:
**	orr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (orr_s64_m_tied1, svint64_t,
		z0 = svorr_s64_m (p0, z0, z1),
		z0 = svorr_m (p0, z0, z1))

/*
** orr_s64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	orr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (orr_s64_m_tied2, svint64_t,
		z0 = svorr_s64_m (p0, z1, z0),
		z0 = svorr_m (p0, z1, z0))

/*
** orr_s64_m_untied:
**	movprfx	z0, z1
**	orr	z0\.d, p0/m, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (orr_s64_m_untied, svint64_t,
		z0 = svorr_s64_m (p0, z1, z2),
		z0 = svorr_m (p0, z1, z2))

/*
** orr_x0_s64_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	orr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (orr_x0_s64_m_tied1, svint64_t, int64_t,
		 z0 = svorr_n_s64_m (p0, z0, x0),
		 z0 = svorr_m (p0, z0, x0))

/*
** orr_x0_s64_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	orr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (orr_x0_s64_m_untied, svint64_t, int64_t,
		 z0 = svorr_n_s64_m (p0, z1, x0),
		 z0 = svorr_m (p0, z1, x0))

/*
** orr_1_s64_m_tied1:
**	mov	(z[0-9]+\.d), #1
**	orr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (orr_1_s64_m_tied1, svint64_t,
		z0 = svorr_n_s64_m (p0, z0, 1),
		z0 = svorr_m (p0, z0, 1))

/*
** orr_1_s64_m_untied:
**	mov	(z[0-9]+\.d), #1
**	movprfx	z0, z1
**	orr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (orr_1_s64_m_untied, svint64_t,
		z0 = svorr_n_s64_m (p0, z1, 1),
		z0 = svorr_m (p0, z1, 1))

/*
** orr_m2_s64_m:
**	mov	(z[0-9]+\.d), #-2
**	orr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (orr_m2_s64_m, svint64_t,
		z0 = svorr_n_s64_m (p0, z0, -2),
		z0 = svorr_m (p0, z0, -2))

/*
** orr_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	orr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (orr_s64_z_tied1, svint64_t,
		z0 = svorr_s64_z (p0, z0, z1),
		z0 = svorr_z (p0, z0, z1))

/*
** orr_s64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	orr	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (orr_s64_z_tied2, svint64_t,
		z0 = svorr_s64_z (p0, z1, z0),
		z0 = svorr_z (p0, z1, z0))

/*
** orr_s64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	orr	z0\.d, p0/m, z0\.d, z2\.d
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	orr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (orr_s64_z_untied, svint64_t,
		z0 = svorr_s64_z (p0, z1, z2),
		z0 = svorr_z (p0, z1, z2))

/*
** orr_x0_s64_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	orr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (orr_x0_s64_z_tied1, svint64_t, int64_t,
		 z0 = svorr_n_s64_z (p0, z0, x0),
		 z0 = svorr_z (p0, z0, x0))

/*
** orr_x0_s64_z_untied:
**	mov	(z[0-9]+\.d), x0
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	orr	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	orr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZX (orr_x0_s64_z_untied, svint64_t, int64_t,
		 z0 = svorr_n_s64_z (p0, z1, x0),
		 z0 = svorr_z (p0, z1, x0))

/*
** orr_1_s64_z_tied1:
**	mov	(z[0-9]+\.d), #1
**	movprfx	z0\.d, p0/z, z0\.d
**	orr	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (orr_1_s64_z_tied1, svint64_t,
		z0 = svorr_n_s64_z (p0, z0, 1),
		z0 = svorr_z (p0, z0, 1))

/*
** orr_1_s64_z_untied:
**	mov	(z[0-9]+\.d), #1
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	orr	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	orr	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (orr_1_s64_z_untied, svint64_t,
		z0 = svorr_n_s64_z (p0, z1, 1),
		z0 = svorr_z (p0, z1, 1))

/*
** orr_s64_x_tied1:
**	orr	z0\.d, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (orr_s64_x_tied1, svint64_t,
		z0 = svorr_s64_x (p0, z0, z1),
		z0 = svorr_x (p0, z0, z1))

/*
** orr_s64_x_tied2:
**	orr	z0\.d, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (orr_s64_x_tied2, svint64_t,
		z0 = svorr_s64_x (p0, z1, z0),
		z0 = svorr_x (p0, z1, z0))

/*
** orr_s64_x_untied:
**	orr	z0\.d, (z1\.d, z2\.d|z2\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_Z (orr_s64_x_untied, svint64_t,
		z0 = svorr_s64_x (p0, z1, z2),
		z0 = svorr_x (p0, z1, z2))

/*
** orr_x0_s64_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	orr	z0\.d, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_UNIFORM_ZX (orr_x0_s64_x_tied1, svint64_t, int64_t,
		 z0 = svorr_n_s64_x (p0, z0, x0),
		 z0 = svorr_x (p0, z0, x0))

/*
** orr_x0_s64_x_untied:
**	mov	(z[0-9]+\.d), x0
**	orr	z0\.d, (z1\.d, \1|\1, z1\.d)
**	ret
*/
TEST_UNIFORM_ZX (orr_x0_s64_x_untied, svint64_t, int64_t,
		 z0 = svorr_n_s64_x (p0, z1, x0),
		 z0 = svorr_x (p0, z1, x0))

/*
** orr_1_s64_x_tied1:
**	orr	z0\.d, z0\.d, #0x1
**	ret
*/
TEST_UNIFORM_Z (orr_1_s64_x_tied1, svint64_t,
		z0 = svorr_n_s64_x (p0, z0, 1),
		z0 = svorr_x (p0, z0, 1))

/*
** orr_1_s64_x_untied:
**	movprfx	z0, z1
**	orr	z0\.d, z0\.d, #0x1
**	ret
*/
TEST_UNIFORM_Z (orr_1_s64_x_untied, svint64_t,
		z0 = svorr_n_s64_x (p0, z1, 1),
		z0 = svorr_x (p0, z1, 1))

/*
** orr_127_s64_x:
**	orr	z0\.d, z0\.d, #0x7f
**	ret
*/
TEST_UNIFORM_Z (orr_127_s64_x, svint64_t,
		z0 = svorr_n_s64_x (p0, z0, 127),
		z0 = svorr_x (p0, z0, 127))

/*
** orr_128_s64_x:
**	orr	z0\.d, z0\.d, #0x80
**	ret
*/
TEST_UNIFORM_Z (orr_128_s64_x, svint64_t,
		z0 = svorr_n_s64_x (p0, z0, 128),
		z0 = svorr_x (p0, z0, 128))

/*
** orr_255_s64_x:
**	orr	z0\.d, z0\.d, #0xff
**	ret
*/
TEST_UNIFORM_Z (orr_255_s64_x, svint64_t,
		z0 = svorr_n_s64_x (p0, z0, 255),
		z0 = svorr_x (p0, z0, 255))

/*
** orr_256_s64_x:
**	orr	z0\.d, z0\.d, #0x100
**	ret
*/
TEST_UNIFORM_Z (orr_256_s64_x, svint64_t,
		z0 = svorr_n_s64_x (p0, z0, 256),
		z0 = svorr_x (p0, z0, 256))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (orr_257_s64_x, svint64_t,
		z0 = svorr_n_s64_x (p0, z0, 257),
		z0 = svorr_x (p0, z0, 257))

/*
** orr_512_s64_x:
**	orr	z0\.d, z0\.d, #0x200
**	ret
*/
TEST_UNIFORM_Z (orr_512_s64_x, svint64_t,
		z0 = svorr_n_s64_x (p0, z0, 512),
		z0 = svorr_x (p0, z0, 512))

/*
** orr_65280_s64_x:
**	orr	z0\.d, z0\.d, #0xff00
**	ret
*/
TEST_UNIFORM_Z (orr_65280_s64_x, svint64_t,
		z0 = svorr_n_s64_x (p0, z0, 0xff00),
		z0 = svorr_x (p0, z0, 0xff00))

/*
** orr_m127_s64_x:
**	orr	z0\.d, z0\.d, #0xffffffffffffff81
**	ret
*/
TEST_UNIFORM_Z (orr_m127_s64_x, svint64_t,
		z0 = svorr_n_s64_x (p0, z0, -127),
		z0 = svorr_x (p0, z0, -127))

/*
** orr_m128_s64_x:
**	orr	z0\.d, z0\.d, #0xffffffffffffff80
**	ret
*/
TEST_UNIFORM_Z (orr_m128_s64_x, svint64_t,
		z0 = svorr_n_s64_x (p0, z0, -128),
		z0 = svorr_x (p0, z0, -128))

/*
** orr_m255_s64_x:
**	orr	z0\.d, z0\.d, #0xffffffffffffff01
**	ret
*/
TEST_UNIFORM_Z (orr_m255_s64_x, svint64_t,
		z0 = svorr_n_s64_x (p0, z0, -255),
		z0 = svorr_x (p0, z0, -255))

/*
** orr_m256_s64_x:
**	orr	z0\.d, z0\.d, #0xffffffffffffff00
**	ret
*/
TEST_UNIFORM_Z (orr_m256_s64_x, svint64_t,
		z0 = svorr_n_s64_x (p0, z0, -256),
		z0 = svorr_x (p0, z0, -256))

/*
** orr_m257_s64_x:
**	orr	z0\.d, z0\.d, #0xfffffffffffffeff
**	ret
*/
TEST_UNIFORM_Z (orr_m257_s64_x, svint64_t,
		z0 = svorr_n_s64_x (p0, z0, -257),
		z0 = svorr_x (p0, z0, -257))

/*
** orr_m512_s64_x:
**	orr	z0\.d, z0\.d, #0xfffffffffffffe00
**	ret
*/
TEST_UNIFORM_Z (orr_m512_s64_x, svint64_t,
		z0 = svorr_n_s64_x (p0, z0, -512),
		z0 = svorr_x (p0, z0, -512))

/*
** orr_m32768_s64_x:
**	orr	z0\.d, z0\.d, #0xffffffffffff8000
**	ret
*/
TEST_UNIFORM_Z (orr_m32768_s64_x, svint64_t,
		z0 = svorr_n_s64_x (p0, z0, -0x8000),
		z0 = svorr_x (p0, z0, -0x8000))

/*
** orr_5_s64_x:
**	mov	(z[0-9]+\.d), #5
**	orr	z0\.d, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (orr_5_s64_x, svint64_t,
		z0 = svorr_n_s64_x (p0, z0, 5),
		z0 = svorr_x (p0, z0, 5))
