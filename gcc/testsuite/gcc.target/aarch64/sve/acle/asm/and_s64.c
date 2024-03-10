/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** and_s64_m_tied1:
**	and	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (and_s64_m_tied1, svint64_t,
		z0 = svand_s64_m (p0, z0, z1),
		z0 = svand_m (p0, z0, z1))

/*
** and_s64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	and	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (and_s64_m_tied2, svint64_t,
		z0 = svand_s64_m (p0, z1, z0),
		z0 = svand_m (p0, z1, z0))

/*
** and_s64_m_untied:
**	movprfx	z0, z1
**	and	z0\.d, p0/m, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (and_s64_m_untied, svint64_t,
		z0 = svand_s64_m (p0, z1, z2),
		z0 = svand_m (p0, z1, z2))

/*
** and_x0_s64_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	and	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (and_x0_s64_m_tied1, svint64_t, int64_t,
		 z0 = svand_n_s64_m (p0, z0, x0),
		 z0 = svand_m (p0, z0, x0))

/*
** and_x0_s64_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	and	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (and_x0_s64_m_untied, svint64_t, int64_t,
		 z0 = svand_n_s64_m (p0, z1, x0),
		 z0 = svand_m (p0, z1, x0))

/*
** and_1_s64_m_tied1:
**	mov	(z[0-9]+\.d), #1
**	and	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (and_1_s64_m_tied1, svint64_t,
		z0 = svand_n_s64_m (p0, z0, 1),
		z0 = svand_m (p0, z0, 1))

/*
** and_1_s64_m_untied:
**	mov	(z[0-9]+\.d), #1
**	movprfx	z0, z1
**	and	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (and_1_s64_m_untied, svint64_t,
		z0 = svand_n_s64_m (p0, z1, 1),
		z0 = svand_m (p0, z1, 1))

/*
** and_m2_s64_m:
**	mov	(z[0-9]+\.d), #-2
**	and	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (and_m2_s64_m, svint64_t,
		z0 = svand_n_s64_m (p0, z0, -2),
		z0 = svand_m (p0, z0, -2))

/*
** and_255_s64_m_tied1:
**	uxtb	z0\.d, p0/m, z0\.d
**	ret
*/
TEST_UNIFORM_Z (and_255_s64_m_tied1, svint64_t,
		z0 = svand_n_s64_m (p0, z0, 255),
		z0 = svand_m (p0, z0, 255))

/*
** and_255_s64_m_untied:
**	movprfx	z0, z1
**	uxtb	z0\.d, p0/m, z1\.d
**	ret
*/
TEST_UNIFORM_Z (and_255_s64_m_untied, svint64_t,
		z0 = svand_n_s64_m (p0, z1, 255),
		z0 = svand_m (p0, z1, 255))

/*
** and_65535_s64_m_tied1:
**	uxth	z0\.d, p0/m, z0\.d
**	ret
*/
TEST_UNIFORM_Z (and_65535_s64_m_tied1, svint64_t,
		z0 = svand_n_s64_m (p0, z0, 65535),
		z0 = svand_m (p0, z0, 65535))

/*
** and_65535_s64_m_untied:
**	movprfx	z0, z1
**	uxth	z0\.d, p0/m, z1\.d
**	ret
*/
TEST_UNIFORM_Z (and_65535_s64_m_untied, svint64_t,
		z0 = svand_n_s64_m (p0, z1, 65535),
		z0 = svand_m (p0, z1, 65535))

/*
** and_0xffffffff_s64_m_tied1:
**	uxtw	z0\.d, p0/m, z0\.d
**	ret
*/
TEST_UNIFORM_Z (and_0xffffffff_s64_m_tied1, svint64_t,
		z0 = svand_n_s64_m (p0, z0, 0xffffffff),
		z0 = svand_m (p0, z0, 0xffffffff))

/*
** and_0xffffffff_s64_m_untied:
**	movprfx	z0, z1
**	uxtw	z0\.d, p0/m, z1\.d
**	ret
*/
TEST_UNIFORM_Z (and_0xffffffff_s64_m_untied, svint64_t,
		z0 = svand_n_s64_m (p0, z1, 0xffffffff),
		z0 = svand_m (p0, z1, 0xffffffff))

/*
** and_s64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	and	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (and_s64_z_tied1, svint64_t,
		z0 = svand_s64_z (p0, z0, z1),
		z0 = svand_z (p0, z0, z1))

/*
** and_s64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	and	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (and_s64_z_tied2, svint64_t,
		z0 = svand_s64_z (p0, z1, z0),
		z0 = svand_z (p0, z1, z0))

/*
** and_s64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	and	z0\.d, p0/m, z0\.d, z2\.d
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	and	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (and_s64_z_untied, svint64_t,
		z0 = svand_s64_z (p0, z1, z2),
		z0 = svand_z (p0, z1, z2))

/*
** and_x0_s64_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	and	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (and_x0_s64_z_tied1, svint64_t, int64_t,
		 z0 = svand_n_s64_z (p0, z0, x0),
		 z0 = svand_z (p0, z0, x0))

/*
** and_x0_s64_z_untied:
**	mov	(z[0-9]+\.d), x0
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	and	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	and	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZX (and_x0_s64_z_untied, svint64_t, int64_t,
		 z0 = svand_n_s64_z (p0, z1, x0),
		 z0 = svand_z (p0, z1, x0))

/*
** and_1_s64_z_tied1:
**	mov	(z[0-9]+\.d), #1
**	movprfx	z0\.d, p0/z, z0\.d
**	and	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (and_1_s64_z_tied1, svint64_t,
		z0 = svand_n_s64_z (p0, z0, 1),
		z0 = svand_z (p0, z0, 1))

/*
** and_1_s64_z_untied:
**	mov	(z[0-9]+\.d), #1
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	and	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	and	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (and_1_s64_z_untied, svint64_t,
		z0 = svand_n_s64_z (p0, z1, 1),
		z0 = svand_z (p0, z1, 1))

/*
** and_255_s64_z_tied1:
** (
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.d, p0/z, \1
**	uxtb	z0\.d, p0/m, \1
** |
**	mov	(z[0-9]+\.d), #255
**	movprfx	z0\.d, p0/z, z0\.d
**	and	z0\.d, p0/m, z0\.d, \1
** )
**	ret
*/
TEST_UNIFORM_Z (and_255_s64_z_tied1, svint64_t,
		z0 = svand_n_s64_z (p0, z0, 255),
		z0 = svand_z (p0, z0, 255))

/*
** and_255_s64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	uxtb	z0\.d, p0/m, z1\.d
**	ret
*/
TEST_UNIFORM_Z (and_255_s64_z_untied, svint64_t,
		z0 = svand_n_s64_z (p0, z1, 255),
		z0 = svand_z (p0, z1, 255))

/*
** and_65535_s64_z_tied1:
** (
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.d, p0/z, \1
**	uxth	z0\.d, p0/m, \1
** |
**	mov	(z[0-9]+\.d), #65535
**	movprfx	z0\.d, p0/z, z0\.d
**	and	z0\.d, p0/m, z0\.d, \1
** )
**	ret
*/
TEST_UNIFORM_Z (and_65535_s64_z_tied1, svint64_t,
		z0 = svand_n_s64_z (p0, z0, 65535),
		z0 = svand_z (p0, z0, 65535))

/*
** and_65535_s64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	uxth	z0\.d, p0/m, z1\.d
**	ret
*/
TEST_UNIFORM_Z (and_65535_s64_z_untied, svint64_t,
		z0 = svand_n_s64_z (p0, z1, 65535),
		z0 = svand_z (p0, z1, 65535))

/*
** and_0xffffffff_s64_z_tied1:
** (
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0\.d, p0/z, \1
**	uxtw	z0\.d, p0/m, \1
** |
**	mov	(z[0-9]+\.d), #4294967295
**	movprfx	z0\.d, p0/z, z0\.d
**	and	z0\.d, p0/m, z0\.d, \1
** )
**	ret
*/
TEST_UNIFORM_Z (and_0xffffffff_s64_z_tied1, svint64_t,
		z0 = svand_n_s64_z (p0, z0, 0xffffffff),
		z0 = svand_z (p0, z0, 0xffffffff))

/*
** and_0xffffffff_s64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	uxtw	z0\.d, p0/m, z1\.d
**	ret
*/
TEST_UNIFORM_Z (and_0xffffffff_s64_z_untied, svint64_t,
		z0 = svand_n_s64_z (p0, z1, 0xffffffff),
		z0 = svand_z (p0, z1, 0xffffffff))

/*
** and_s64_x_tied1:
**	and	z0\.d, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (and_s64_x_tied1, svint64_t,
		z0 = svand_s64_x (p0, z0, z1),
		z0 = svand_x (p0, z0, z1))

/*
** and_s64_x_tied2:
**	and	z0\.d, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (and_s64_x_tied2, svint64_t,
		z0 = svand_s64_x (p0, z1, z0),
		z0 = svand_x (p0, z1, z0))

/*
** and_s64_x_untied:
**	and	z0\.d, (z1\.d, z2\.d|z2\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_Z (and_s64_x_untied, svint64_t,
		z0 = svand_s64_x (p0, z1, z2),
		z0 = svand_x (p0, z1, z2))

/*
** and_x0_s64_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	and	z0\.d, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_UNIFORM_ZX (and_x0_s64_x_tied1, svint64_t, int64_t,
		 z0 = svand_n_s64_x (p0, z0, x0),
		 z0 = svand_x (p0, z0, x0))

/*
** and_x0_s64_x_untied:
**	mov	(z[0-9]+\.d), x0
**	and	z0\.d, (z1\.d, \1|\1, z1\.d)
**	ret
*/
TEST_UNIFORM_ZX (and_x0_s64_x_untied, svint64_t, int64_t,
		 z0 = svand_n_s64_x (p0, z1, x0),
		 z0 = svand_x (p0, z1, x0))

/*
** and_1_s64_x_tied1:
**	and	z0\.d, z0\.d, #0x1
**	ret
*/
TEST_UNIFORM_Z (and_1_s64_x_tied1, svint64_t,
		z0 = svand_n_s64_x (p0, z0, 1),
		z0 = svand_x (p0, z0, 1))

/*
** and_1_s64_x_untied:
**	movprfx	z0, z1
**	and	z0\.d, z0\.d, #0x1
**	ret
*/
TEST_UNIFORM_Z (and_1_s64_x_untied, svint64_t,
		z0 = svand_n_s64_x (p0, z1, 1),
		z0 = svand_x (p0, z1, 1))

/*
** and_127_s64_x:
**	and	z0\.d, z0\.d, #0x7f
**	ret
*/
TEST_UNIFORM_Z (and_127_s64_x, svint64_t,
		z0 = svand_n_s64_x (p0, z0, 127),
		z0 = svand_x (p0, z0, 127))

/*
** and_128_s64_x:
**	and	z0\.d, z0\.d, #0x80
**	ret
*/
TEST_UNIFORM_Z (and_128_s64_x, svint64_t,
		z0 = svand_n_s64_x (p0, z0, 128),
		z0 = svand_x (p0, z0, 128))

/*
** and_255_s64_x:
**	and	z0\.d, z0\.d, #0xff
**	ret
*/
TEST_UNIFORM_Z (and_255_s64_x, svint64_t,
		z0 = svand_n_s64_x (p0, z0, 255),
		z0 = svand_x (p0, z0, 255))

/*
** and_256_s64_x:
**	and	z0\.d, z0\.d, #0x100
**	ret
*/
TEST_UNIFORM_Z (and_256_s64_x, svint64_t,
		z0 = svand_n_s64_x (p0, z0, 256),
		z0 = svand_x (p0, z0, 256))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (and_257_s64_x, svint64_t,
		z0 = svand_n_s64_x (p0, z0, 257),
		z0 = svand_x (p0, z0, 257))

/*
** and_512_s64_x:
**	and	z0\.d, z0\.d, #0x200
**	ret
*/
TEST_UNIFORM_Z (and_512_s64_x, svint64_t,
		z0 = svand_n_s64_x (p0, z0, 512),
		z0 = svand_x (p0, z0, 512))

/*
** and_65280_s64_x:
**	and	z0\.d, z0\.d, #0xff00
**	ret
*/
TEST_UNIFORM_Z (and_65280_s64_x, svint64_t,
		z0 = svand_n_s64_x (p0, z0, 0xff00),
		z0 = svand_x (p0, z0, 0xff00))

/*
** and_m127_s64_x:
**	and	z0\.d, z0\.d, #0xffffffffffffff81
**	ret
*/
TEST_UNIFORM_Z (and_m127_s64_x, svint64_t,
		z0 = svand_n_s64_x (p0, z0, -127),
		z0 = svand_x (p0, z0, -127))

/*
** and_m128_s64_x:
**	and	z0\.d, z0\.d, #0xffffffffffffff80
**	ret
*/
TEST_UNIFORM_Z (and_m128_s64_x, svint64_t,
		z0 = svand_n_s64_x (p0, z0, -128),
		z0 = svand_x (p0, z0, -128))

/*
** and_m255_s64_x:
**	and	z0\.d, z0\.d, #0xffffffffffffff01
**	ret
*/
TEST_UNIFORM_Z (and_m255_s64_x, svint64_t,
		z0 = svand_n_s64_x (p0, z0, -255),
		z0 = svand_x (p0, z0, -255))

/*
** and_m256_s64_x:
**	and	z0\.d, z0\.d, #0xffffffffffffff00
**	ret
*/
TEST_UNIFORM_Z (and_m256_s64_x, svint64_t,
		z0 = svand_n_s64_x (p0, z0, -256),
		z0 = svand_x (p0, z0, -256))

/*
** and_m257_s64_x:
**	and	z0\.d, z0\.d, #0xfffffffffffffeff
**	ret
*/
TEST_UNIFORM_Z (and_m257_s64_x, svint64_t,
		z0 = svand_n_s64_x (p0, z0, -257),
		z0 = svand_x (p0, z0, -257))

/*
** and_m512_s64_x:
**	and	z0\.d, z0\.d, #0xfffffffffffffe00
**	ret
*/
TEST_UNIFORM_Z (and_m512_s64_x, svint64_t,
		z0 = svand_n_s64_x (p0, z0, -512),
		z0 = svand_x (p0, z0, -512))

/*
** and_m32768_s64_x:
**	and	z0\.d, z0\.d, #0xffffffffffff8000
**	ret
*/
TEST_UNIFORM_Z (and_m32768_s64_x, svint64_t,
		z0 = svand_n_s64_x (p0, z0, -0x8000),
		z0 = svand_x (p0, z0, -0x8000))

/*
** and_5_s64_x:
**	mov	(z[0-9]+\.d), #5
**	and	z0\.d, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (and_5_s64_x, svint64_t,
		z0 = svand_n_s64_x (p0, z0, 5),
		z0 = svand_x (p0, z0, 5))
