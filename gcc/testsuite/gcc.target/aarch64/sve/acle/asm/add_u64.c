/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** add_u64_m_tied1:
**	add	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (add_u64_m_tied1, svuint64_t,
		z0 = svadd_u64_m (p0, z0, z1),
		z0 = svadd_m (p0, z0, z1))

/*
** add_u64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	add	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (add_u64_m_tied2, svuint64_t,
		z0 = svadd_u64_m (p0, z1, z0),
		z0 = svadd_m (p0, z1, z0))

/*
** add_u64_m_untied:
**	movprfx	z0, z1
**	add	z0\.d, p0/m, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (add_u64_m_untied, svuint64_t,
		z0 = svadd_u64_m (p0, z1, z2),
		z0 = svadd_m (p0, z1, z2))

/*
** add_x0_u64_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	add	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (add_x0_u64_m_tied1, svuint64_t, uint64_t,
		 z0 = svadd_n_u64_m (p0, z0, x0),
		 z0 = svadd_m (p0, z0, x0))

/*
** add_x0_u64_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	add	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (add_x0_u64_m_untied, svuint64_t, uint64_t,
		 z0 = svadd_n_u64_m (p0, z1, x0),
		 z0 = svadd_m (p0, z1, x0))

/*
** add_1_u64_m_tied1:
**	mov	(z[0-9]+\.d), #1
**	add	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (add_1_u64_m_tied1, svuint64_t,
		z0 = svadd_n_u64_m (p0, z0, 1),
		z0 = svadd_m (p0, z0, 1))

/*
** add_1_u64_m_untied:
**	mov	(z[0-9]+\.d), #1
**	movprfx	z0, z1
**	add	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (add_1_u64_m_untied, svuint64_t,
		z0 = svadd_n_u64_m (p0, z1, 1),
		z0 = svadd_m (p0, z1, 1))

/*
** add_m2_u64_m:
**	mov	(z[0-9]+\.d), #-2
**	add	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (add_m2_u64_m, svuint64_t,
		z0 = svadd_n_u64_m (p0, z0, -2),
		z0 = svadd_m (p0, z0, -2))

/*
** add_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	add	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (add_u64_z_tied1, svuint64_t,
		z0 = svadd_u64_z (p0, z0, z1),
		z0 = svadd_z (p0, z0, z1))

/*
** add_u64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	add	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (add_u64_z_tied2, svuint64_t,
		z0 = svadd_u64_z (p0, z1, z0),
		z0 = svadd_z (p0, z1, z0))

/*
** add_u64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	add	z0\.d, p0/m, z0\.d, z2\.d
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	add	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (add_u64_z_untied, svuint64_t,
		z0 = svadd_u64_z (p0, z1, z2),
		z0 = svadd_z (p0, z1, z2))

/*
** add_x0_u64_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	add	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (add_x0_u64_z_tied1, svuint64_t, uint64_t,
		 z0 = svadd_n_u64_z (p0, z0, x0),
		 z0 = svadd_z (p0, z0, x0))

/*
** add_x0_u64_z_untied:
**	mov	(z[0-9]+\.d), x0
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	add	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	add	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZX (add_x0_u64_z_untied, svuint64_t, uint64_t,
		 z0 = svadd_n_u64_z (p0, z1, x0),
		 z0 = svadd_z (p0, z1, x0))

/*
** add_1_u64_z_tied1:
**	mov	(z[0-9]+\.d), #1
**	movprfx	z0\.d, p0/z, z0\.d
**	add	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (add_1_u64_z_tied1, svuint64_t,
		z0 = svadd_n_u64_z (p0, z0, 1),
		z0 = svadd_z (p0, z0, 1))

/*
** add_1_u64_z_untied:
**	mov	(z[0-9]+\.d), #1
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	add	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	add	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (add_1_u64_z_untied, svuint64_t,
		z0 = svadd_n_u64_z (p0, z1, 1),
		z0 = svadd_z (p0, z1, 1))

/*
** add_u64_x_tied1:
**	add	z0\.d, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (add_u64_x_tied1, svuint64_t,
		z0 = svadd_u64_x (p0, z0, z1),
		z0 = svadd_x (p0, z0, z1))

/*
** add_u64_x_tied2:
**	add	z0\.d, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (add_u64_x_tied2, svuint64_t,
		z0 = svadd_u64_x (p0, z1, z0),
		z0 = svadd_x (p0, z1, z0))

/*
** add_u64_x_untied:
**	add	z0\.d, (z1\.d, z2\.d|z2\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_Z (add_u64_x_untied, svuint64_t,
		z0 = svadd_u64_x (p0, z1, z2),
		z0 = svadd_x (p0, z1, z2))

/*
** add_x0_u64_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	add	z0\.d, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_UNIFORM_ZX (add_x0_u64_x_tied1, svuint64_t, uint64_t,
		 z0 = svadd_n_u64_x (p0, z0, x0),
		 z0 = svadd_x (p0, z0, x0))

/*
** add_x0_u64_x_untied:
**	mov	(z[0-9]+\.d), x0
**	add	z0\.d, (z1\.d, \1|\1, z1\.d)
**	ret
*/
TEST_UNIFORM_ZX (add_x0_u64_x_untied, svuint64_t, uint64_t,
		 z0 = svadd_n_u64_x (p0, z1, x0),
		 z0 = svadd_x (p0, z1, x0))

/*
** add_1_u64_x_tied1:
**	add	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (add_1_u64_x_tied1, svuint64_t,
		z0 = svadd_n_u64_x (p0, z0, 1),
		z0 = svadd_x (p0, z0, 1))

/*
** add_1_u64_x_untied:
**	movprfx	z0, z1
**	add	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (add_1_u64_x_untied, svuint64_t,
		z0 = svadd_n_u64_x (p0, z1, 1),
		z0 = svadd_x (p0, z1, 1))

/*
** add_127_u64_x:
**	add	z0\.d, z0\.d, #127
**	ret
*/
TEST_UNIFORM_Z (add_127_u64_x, svuint64_t,
		z0 = svadd_n_u64_x (p0, z0, 127),
		z0 = svadd_x (p0, z0, 127))

/*
** add_128_u64_x:
**	add	z0\.d, z0\.d, #128
**	ret
*/
TEST_UNIFORM_Z (add_128_u64_x, svuint64_t,
		z0 = svadd_n_u64_x (p0, z0, 128),
		z0 = svadd_x (p0, z0, 128))

/*
** add_255_u64_x:
**	add	z0\.d, z0\.d, #255
**	ret
*/
TEST_UNIFORM_Z (add_255_u64_x, svuint64_t,
		z0 = svadd_n_u64_x (p0, z0, 255),
		z0 = svadd_x (p0, z0, 255))

/*
** add_256_u64_x:
**	add	z0\.d, z0\.d, #256
**	ret
*/
TEST_UNIFORM_Z (add_256_u64_x, svuint64_t,
		z0 = svadd_n_u64_x (p0, z0, 256),
		z0 = svadd_x (p0, z0, 256))

/*
** add_511_u64_x:
**	mov	(z[0-9]+\.d), #511
**	add	z0\.d, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (add_511_u64_x, svuint64_t,
		z0 = svadd_n_u64_x (p0, z0, 511),
		z0 = svadd_x (p0, z0, 511))

/*
** add_512_u64_x:
**	add	z0\.d, z0\.d, #512
**	ret
*/
TEST_UNIFORM_Z (add_512_u64_x, svuint64_t,
		z0 = svadd_n_u64_x (p0, z0, 512),
		z0 = svadd_x (p0, z0, 512))

/*
** add_65280_u64_x:
**	add	z0\.d, z0\.d, #65280
**	ret
*/
TEST_UNIFORM_Z (add_65280_u64_x, svuint64_t,
		z0 = svadd_n_u64_x (p0, z0, 0xff00),
		z0 = svadd_x (p0, z0, 0xff00))

/*
** add_65535_u64_x:
**	mov	(z[0-9]+\.d), #65535
**	add	z0\.d, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (add_65535_u64_x, svuint64_t,
		z0 = svadd_n_u64_x (p0, z0, 65535),
		z0 = svadd_x (p0, z0, 65535))

/*
** add_65536_u64_x:
**	mov	(z[0-9]+\.d), #65536
**	add	z0\.d, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (add_65536_u64_x, svuint64_t,
		z0 = svadd_n_u64_x (p0, z0, 65536),
		z0 = svadd_x (p0, z0, 65536))

/*
** add_m1_u64_x:
**	sub	z0\.d, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (add_m1_u64_x, svuint64_t,
		z0 = svadd_n_u64_x (p0, z0, -1),
		z0 = svadd_x (p0, z0, -1))

/*
** add_m127_u64_x:
**	sub	z0\.d, z0\.d, #127
**	ret
*/
TEST_UNIFORM_Z (add_m127_u64_x, svuint64_t,
		z0 = svadd_n_u64_x (p0, z0, -127),
		z0 = svadd_x (p0, z0, -127))

/*
** add_m128_u64_x:
**	sub	z0\.d, z0\.d, #128
**	ret
*/
TEST_UNIFORM_Z (add_m128_u64_x, svuint64_t,
		z0 = svadd_n_u64_x (p0, z0, -128),
		z0 = svadd_x (p0, z0, -128))

/*
** add_m255_u64_x:
**	sub	z0\.d, z0\.d, #255
**	ret
*/
TEST_UNIFORM_Z (add_m255_u64_x, svuint64_t,
		z0 = svadd_n_u64_x (p0, z0, -255),
		z0 = svadd_x (p0, z0, -255))

/*
** add_m256_u64_x:
**	sub	z0\.d, z0\.d, #256
**	ret
*/
TEST_UNIFORM_Z (add_m256_u64_x, svuint64_t,
		z0 = svadd_n_u64_x (p0, z0, -256),
		z0 = svadd_x (p0, z0, -256))

/*
** add_m511_u64_x:
**	mov	(z[0-9]+\.d), #-511
**	add	z0\.d, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (add_m511_u64_x, svuint64_t,
		z0 = svadd_n_u64_x (p0, z0, -511),
		z0 = svadd_x (p0, z0, -511))

/*
** add_m512_u64_x:
**	sub	z0\.d, z0\.d, #512
**	ret
*/
TEST_UNIFORM_Z (add_m512_u64_x, svuint64_t,
		z0 = svadd_n_u64_x (p0, z0, -512),
		z0 = svadd_x (p0, z0, -512))

/*
** add_m32768_u64_x:
**	sub	z0\.d, z0\.d, #32768
**	ret
*/
TEST_UNIFORM_Z (add_m32768_u64_x, svuint64_t,
		z0 = svadd_n_u64_x (p0, z0, -0x8000),
		z0 = svadd_x (p0, z0, -0x8000))

/*
** add_m65280_u64_x:
**	sub	z0\.d, z0\.d, #65280
**	ret
*/
TEST_UNIFORM_Z (add_m65280_u64_x, svuint64_t,
		z0 = svadd_n_u64_x (p0, z0, -0xff00),
		z0 = svadd_x (p0, z0, -0xff00))

/*
** add_m65535_u64_x:
**	mov	(z[0-9]+\.d), #-65535
**	add	z0\.d, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (add_m65535_u64_x, svuint64_t,
		z0 = svadd_n_u64_x (p0, z0, -65535),
		z0 = svadd_x (p0, z0, -65535))

/*
** add_m65536_u64_x:
**	mov	(z[0-9]+\.d), #-65536
**	add	z0\.d, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (add_m65536_u64_x, svuint64_t,
		z0 = svadd_n_u64_x (p0, z0, -65536),
		z0 = svadd_x (p0, z0, -65536))
