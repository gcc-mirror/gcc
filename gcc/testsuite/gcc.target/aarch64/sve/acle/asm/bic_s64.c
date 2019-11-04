/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** bic_s64_m_tied1:
**	bic	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (bic_s64_m_tied1, svint64_t,
		z0 = svbic_s64_m (p0, z0, z1),
		z0 = svbic_m (p0, z0, z1))

/*
** bic_s64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	bic	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (bic_s64_m_tied2, svint64_t,
		z0 = svbic_s64_m (p0, z1, z0),
		z0 = svbic_m (p0, z1, z0))

/*
** bic_s64_m_untied:
**	movprfx	z0, z1
**	bic	z0\.d, p0/m, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (bic_s64_m_untied, svint64_t,
		z0 = svbic_s64_m (p0, z1, z2),
		z0 = svbic_m (p0, z1, z2))

/*
** bic_x0_s64_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	bic	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (bic_x0_s64_m_tied1, svint64_t, int64_t,
		 z0 = svbic_n_s64_m (p0, z0, x0),
		 z0 = svbic_m (p0, z0, x0))

/*
** bic_x0_s64_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	bic	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (bic_x0_s64_m_untied, svint64_t, int64_t,
		 z0 = svbic_n_s64_m (p0, z1, x0),
		 z0 = svbic_m (p0, z1, x0))

/*
** bic_1_s64_m_tied1:
**	mov	(z[0-9]+\.d), #-2
**	and	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (bic_1_s64_m_tied1, svint64_t,
		z0 = svbic_n_s64_m (p0, z0, 1),
		z0 = svbic_m (p0, z0, 1))

/*
** bic_1_s64_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.d), #-2
**	movprfx	z0, z1
**	and	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (bic_1_s64_m_untied, svint64_t,
		z0 = svbic_n_s64_m (p0, z1, 1),
		z0 = svbic_m (p0, z1, 1))

/*
** bic_m2_s64_m:
**	mov	(z[0-9]+\.d), #1
**	and	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (bic_m2_s64_m, svint64_t,
		z0 = svbic_n_s64_m (p0, z0, -2),
		z0 = svbic_m (p0, z0, -2))

/*
** bic_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	bic	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (bic_s64_z_tied1, svint64_t,
		z0 = svbic_s64_z (p0, z0, z1),
		z0 = svbic_z (p0, z0, z1))

/*
** bic_s64_z_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.d, p0/z, z1\.d
**	bic	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (bic_s64_z_tied2, svint64_t,
		z0 = svbic_s64_z (p0, z1, z0),
		z0 = svbic_z (p0, z1, z0))

/*
** bic_s64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	bic	z0\.d, p0/m, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (bic_s64_z_untied, svint64_t,
		z0 = svbic_s64_z (p0, z1, z2),
		z0 = svbic_z (p0, z1, z2))

/*
** bic_x0_s64_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	bic	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (bic_x0_s64_z_tied1, svint64_t, int64_t,
		 z0 = svbic_n_s64_z (p0, z0, x0),
		 z0 = svbic_z (p0, z0, x0))

/*
** bic_x0_s64_z_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z1\.d
**	bic	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (bic_x0_s64_z_untied, svint64_t, int64_t,
		 z0 = svbic_n_s64_z (p0, z1, x0),
		 z0 = svbic_z (p0, z1, x0))

/*
** bic_1_s64_z_tied1:
**	mov	(z[0-9]+\.d), #-2
**	movprfx	z0\.d, p0/z, z0\.d
**	and	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (bic_1_s64_z_tied1, svint64_t,
		z0 = svbic_n_s64_z (p0, z0, 1),
		z0 = svbic_z (p0, z0, 1))

/*
** bic_1_s64_z_untied:
**	mov	(z[0-9]+\.d), #-2
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	and	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	and	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (bic_1_s64_z_untied, svint64_t,
		z0 = svbic_n_s64_z (p0, z1, 1),
		z0 = svbic_z (p0, z1, 1))

/*
** bic_s64_x_tied1:
**	bic	z0\.d, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (bic_s64_x_tied1, svint64_t,
		z0 = svbic_s64_x (p0, z0, z1),
		z0 = svbic_x (p0, z0, z1))

/*
** bic_s64_x_tied2:
**	bic	z0\.d, z1\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (bic_s64_x_tied2, svint64_t,
		z0 = svbic_s64_x (p0, z1, z0),
		z0 = svbic_x (p0, z1, z0))

/*
** bic_s64_x_untied:
**	bic	z0\.d, z1\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (bic_s64_x_untied, svint64_t,
		z0 = svbic_s64_x (p0, z1, z2),
		z0 = svbic_x (p0, z1, z2))

/*
** bic_x0_s64_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	bic	z0\.d, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (bic_x0_s64_x_tied1, svint64_t, int64_t,
		 z0 = svbic_n_s64_x (p0, z0, x0),
		 z0 = svbic_x (p0, z0, x0))

/*
** bic_x0_s64_x_untied:
**	mov	(z[0-9]+\.d), x0
**	bic	z0\.d, z1\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (bic_x0_s64_x_untied, svint64_t, int64_t,
		 z0 = svbic_n_s64_x (p0, z1, x0),
		 z0 = svbic_x (p0, z1, x0))

/*
** bic_1_s64_x_tied1:
**	and	z0\.d, z0\.d, #0xfffffffffffffffe
**	ret
*/
TEST_UNIFORM_Z (bic_1_s64_x_tied1, svint64_t,
		z0 = svbic_n_s64_x (p0, z0, 1),
		z0 = svbic_x (p0, z0, 1))

/*
** bic_1_s64_x_untied:
**	movprfx	z0, z1
**	and	z0\.d, z0\.d, #0xfffffffffffffffe
**	ret
*/
TEST_UNIFORM_Z (bic_1_s64_x_untied, svint64_t,
		z0 = svbic_n_s64_x (p0, z1, 1),
		z0 = svbic_x (p0, z1, 1))

/*
** bic_127_s64_x:
**	and	z0\.d, z0\.d, #0xffffffffffffff80
**	ret
*/
TEST_UNIFORM_Z (bic_127_s64_x, svint64_t,
		z0 = svbic_n_s64_x (p0, z0, 127),
		z0 = svbic_x (p0, z0, 127))

/*
** bic_128_s64_x:
**	and	z0\.d, z0\.d, #0xffffffffffffff7f
**	ret
*/
TEST_UNIFORM_Z (bic_128_s64_x, svint64_t,
		z0 = svbic_n_s64_x (p0, z0, 128),
		z0 = svbic_x (p0, z0, 128))

/*
** bic_255_s64_x:
**	and	z0\.d, z0\.d, #0xffffffffffffff00
**	ret
*/
TEST_UNIFORM_Z (bic_255_s64_x, svint64_t,
		z0 = svbic_n_s64_x (p0, z0, 255),
		z0 = svbic_x (p0, z0, 255))

/*
** bic_256_s64_x:
**	and	z0\.d, z0\.d, #0xfffffffffffffeff
**	ret
*/
TEST_UNIFORM_Z (bic_256_s64_x, svint64_t,
		z0 = svbic_n_s64_x (p0, z0, 256),
		z0 = svbic_x (p0, z0, 256))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (bic_257_s64_x, svint64_t,
		z0 = svbic_n_s64_x (p0, z0, 257),
		z0 = svbic_x (p0, z0, 257))

/*
** bic_512_s64_x:
**	and	z0\.d, z0\.d, #0xfffffffffffffdff
**	ret
*/
TEST_UNIFORM_Z (bic_512_s64_x, svint64_t,
		z0 = svbic_n_s64_x (p0, z0, 512),
		z0 = svbic_x (p0, z0, 512))

/*
** bic_65280_s64_x:
**	and	z0\.d, z0\.d, #0xffffffffffff00ff
**	ret
*/
TEST_UNIFORM_Z (bic_65280_s64_x, svint64_t,
		z0 = svbic_n_s64_x (p0, z0, 0xff00),
		z0 = svbic_x (p0, z0, 0xff00))

/*
** bic_m127_s64_x:
**	and	z0\.d, z0\.d, #0x7e
**	ret
*/
TEST_UNIFORM_Z (bic_m127_s64_x, svint64_t,
		z0 = svbic_n_s64_x (p0, z0, -127),
		z0 = svbic_x (p0, z0, -127))

/*
** bic_m128_s64_x:
**	and	z0\.d, z0\.d, #0x7f
**	ret
*/
TEST_UNIFORM_Z (bic_m128_s64_x, svint64_t,
		z0 = svbic_n_s64_x (p0, z0, -128),
		z0 = svbic_x (p0, z0, -128))

/*
** bic_m255_s64_x:
**	and	z0\.d, z0\.d, #0xfe
**	ret
*/
TEST_UNIFORM_Z (bic_m255_s64_x, svint64_t,
		z0 = svbic_n_s64_x (p0, z0, -255),
		z0 = svbic_x (p0, z0, -255))

/*
** bic_m256_s64_x:
**	and	z0\.d, z0\.d, #0xff
**	ret
*/
TEST_UNIFORM_Z (bic_m256_s64_x, svint64_t,
		z0 = svbic_n_s64_x (p0, z0, -256),
		z0 = svbic_x (p0, z0, -256))

/*
** bic_m257_s64_x:
**	and	z0\.d, z0\.d, #0x100
**	ret
*/
TEST_UNIFORM_Z (bic_m257_s64_x, svint64_t,
		z0 = svbic_n_s64_x (p0, z0, -257),
		z0 = svbic_x (p0, z0, -257))

/*
** bic_m512_s64_x:
**	and	z0\.d, z0\.d, #0x1ff
**	ret
*/
TEST_UNIFORM_Z (bic_m512_s64_x, svint64_t,
		z0 = svbic_n_s64_x (p0, z0, -512),
		z0 = svbic_x (p0, z0, -512))

/*
** bic_m32768_s64_x:
**	and	z0\.d, z0\.d, #0x7fff
**	ret
*/
TEST_UNIFORM_Z (bic_m32768_s64_x, svint64_t,
		z0 = svbic_n_s64_x (p0, z0, -0x8000),
		z0 = svbic_x (p0, z0, -0x8000))

/*
** bic_5_s64_x:
**	mov	(z[0-9]+\.d), #-6
**	and	z0\.d, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (bic_5_s64_x, svint64_t,
		z0 = svbic_n_s64_x (p0, z0, 5),
		z0 = svbic_x (p0, z0, 5))
