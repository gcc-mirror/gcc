/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** and_s32_m_tied1:
**	and	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (and_s32_m_tied1, svint32_t,
		z0 = svand_s32_m (p0, z0, z1),
		z0 = svand_m (p0, z0, z1))

/*
** and_s32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	and	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (and_s32_m_tied2, svint32_t,
		z0 = svand_s32_m (p0, z1, z0),
		z0 = svand_m (p0, z1, z0))

/*
** and_s32_m_untied:
**	movprfx	z0, z1
**	and	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (and_s32_m_untied, svint32_t,
		z0 = svand_s32_m (p0, z1, z2),
		z0 = svand_m (p0, z1, z2))

/*
** and_w0_s32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	and	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (and_w0_s32_m_tied1, svint32_t, int32_t,
		 z0 = svand_n_s32_m (p0, z0, x0),
		 z0 = svand_m (p0, z0, x0))

/*
** and_w0_s32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	and	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (and_w0_s32_m_untied, svint32_t, int32_t,
		 z0 = svand_n_s32_m (p0, z1, x0),
		 z0 = svand_m (p0, z1, x0))

/*
** and_1_s32_m_tied1:
**	mov	(z[0-9]+\.s), #1
**	and	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (and_1_s32_m_tied1, svint32_t,
		z0 = svand_n_s32_m (p0, z0, 1),
		z0 = svand_m (p0, z0, 1))

/*
** and_1_s32_m_untied:
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0, z1
**	and	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (and_1_s32_m_untied, svint32_t,
		z0 = svand_n_s32_m (p0, z1, 1),
		z0 = svand_m (p0, z1, 1))

/*
** and_m2_s32_m:
**	mov	(z[0-9]+\.s), #-2
**	and	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (and_m2_s32_m, svint32_t,
		z0 = svand_n_s32_m (p0, z0, -2),
		z0 = svand_m (p0, z0, -2))

/*
** and_255_s32_m_tied1:
**	uxtb	z0\.s, p0/m, z0\.s
**	ret
*/
TEST_UNIFORM_Z (and_255_s32_m_tied1, svint32_t,
		z0 = svand_n_s32_m (p0, z0, 255),
		z0 = svand_m (p0, z0, 255))

/*
** and_255_s32_m_untied:
**	movprfx	z0, z1
**	uxtb	z0\.s, p0/m, z1\.s
**	ret
*/
TEST_UNIFORM_Z (and_255_s32_m_untied, svint32_t,
		z0 = svand_n_s32_m (p0, z1, 255),
		z0 = svand_m (p0, z1, 255))

/*
** and_65535_s32_m_tied1:
**	uxth	z0\.s, p0/m, z0\.s
**	ret
*/
TEST_UNIFORM_Z (and_65535_s32_m_tied1, svint32_t,
		z0 = svand_n_s32_m (p0, z0, 65535),
		z0 = svand_m (p0, z0, 65535))

/*
** and_65535_s32_m_untied:
**	movprfx	z0, z1
**	uxth	z0\.s, p0/m, z1\.s
**	ret
*/
TEST_UNIFORM_Z (and_65535_s32_m_untied, svint32_t,
		z0 = svand_n_s32_m (p0, z1, 65535),
		z0 = svand_m (p0, z1, 65535))

/*
** and_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	and	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (and_s32_z_tied1, svint32_t,
		z0 = svand_s32_z (p0, z0, z1),
		z0 = svand_z (p0, z0, z1))

/*
** and_s32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	and	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (and_s32_z_tied2, svint32_t,
		z0 = svand_s32_z (p0, z1, z0),
		z0 = svand_z (p0, z1, z0))

/*
** and_s32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	and	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	and	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (and_s32_z_untied, svint32_t,
		z0 = svand_s32_z (p0, z1, z2),
		z0 = svand_z (p0, z1, z2))

/*
** and_w0_s32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	and	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (and_w0_s32_z_tied1, svint32_t, int32_t,
		 z0 = svand_n_s32_z (p0, z0, x0),
		 z0 = svand_z (p0, z0, x0))

/*
** and_w0_s32_z_untied:
**	mov	(z[0-9]+\.s), w0
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	and	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	and	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZX (and_w0_s32_z_untied, svint32_t, int32_t,
		 z0 = svand_n_s32_z (p0, z1, x0),
		 z0 = svand_z (p0, z1, x0))

/*
** and_1_s32_z_tied1:
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0\.s, p0/z, z0\.s
**	and	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (and_1_s32_z_tied1, svint32_t,
		z0 = svand_n_s32_z (p0, z0, 1),
		z0 = svand_z (p0, z0, 1))

/*
** and_1_s32_z_untied:
**	mov	(z[0-9]+\.s), #1
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	and	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	and	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (and_1_s32_z_untied, svint32_t,
		z0 = svand_n_s32_z (p0, z1, 1),
		z0 = svand_z (p0, z1, 1))

/*
** and_255_s32_z_tied1:
** (
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.s, p0/z, \1\.s
**	uxtb	z0\.s, p0/m, \1\.s
** |
**	mov	(z[0-9]+\.s), #255
**	movprfx	z0\.s, p0/z, z0\.s
**	and	z0\.s, p0/m, z0\.s, \1
** )
**	ret
*/
TEST_UNIFORM_Z (and_255_s32_z_tied1, svint32_t,
		z0 = svand_n_s32_z (p0, z0, 255),
		z0 = svand_z (p0, z0, 255))

/*
** and_255_s32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	uxtb	z0\.s, p0/m, z1\.s
**	ret
*/
TEST_UNIFORM_Z (and_255_s32_z_untied, svint32_t,
		z0 = svand_n_s32_z (p0, z1, 255),
		z0 = svand_z (p0, z1, 255))

/*
** and_65535_s32_z_tied1:
** (
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0\.s, p0/z, \1\.s
**	uxth	z0\.s, p0/m, \1\.s
** |
**	mov	(z[0-9]+\.s), #65535
**	movprfx	z0\.s, p0/z, z0\.s
**	and	z0\.s, p0/m, z0\.s, \1
** )
**	ret
*/
TEST_UNIFORM_Z (and_65535_s32_z_tied1, svint32_t,
		z0 = svand_n_s32_z (p0, z0, 65535),
		z0 = svand_z (p0, z0, 65535))

/*
** and_65535_s32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	uxth	z0\.s, p0/m, z1\.s
**	ret
*/
TEST_UNIFORM_Z (and_65535_s32_z_untied, svint32_t,
		z0 = svand_n_s32_z (p0, z1, 65535),
		z0 = svand_z (p0, z1, 65535))

/*
** and_s32_x_tied1:
**	and	z0\.d, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (and_s32_x_tied1, svint32_t,
		z0 = svand_s32_x (p0, z0, z1),
		z0 = svand_x (p0, z0, z1))

/*
** and_s32_x_tied2:
**	and	z0\.d, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (and_s32_x_tied2, svint32_t,
		z0 = svand_s32_x (p0, z1, z0),
		z0 = svand_x (p0, z1, z0))

/*
** and_s32_x_untied:
**	and	z0\.d, (z1\.d, z2\.d|z2\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_Z (and_s32_x_untied, svint32_t,
		z0 = svand_s32_x (p0, z1, z2),
		z0 = svand_x (p0, z1, z2))

/*
** and_w0_s32_x_tied1:
**	mov	(z[0-9]+)\.s, w0
**	and	z0\.d, (z0\.d, \1\.d|\1\.d, z0\.d)
**	ret
*/
TEST_UNIFORM_ZX (and_w0_s32_x_tied1, svint32_t, int32_t,
		 z0 = svand_n_s32_x (p0, z0, x0),
		 z0 = svand_x (p0, z0, x0))

/*
** and_w0_s32_x_untied:
**	mov	(z[0-9]+)\.s, w0
**	and	z0\.d, (z1\.d, \1\.d|\1\.d, z1\.d)
**	ret
*/
TEST_UNIFORM_ZX (and_w0_s32_x_untied, svint32_t, int32_t,
		 z0 = svand_n_s32_x (p0, z1, x0),
		 z0 = svand_x (p0, z1, x0))

/*
** and_1_s32_x_tied1:
**	and	z0\.s, z0\.s, #0x1
**	ret
*/
TEST_UNIFORM_Z (and_1_s32_x_tied1, svint32_t,
		z0 = svand_n_s32_x (p0, z0, 1),
		z0 = svand_x (p0, z0, 1))

/*
** and_1_s32_x_untied:
**	movprfx	z0, z1
**	and	z0\.s, z0\.s, #0x1
**	ret
*/
TEST_UNIFORM_Z (and_1_s32_x_untied, svint32_t,
		z0 = svand_n_s32_x (p0, z1, 1),
		z0 = svand_x (p0, z1, 1))

/*
** and_127_s32_x:
**	and	z0\.s, z0\.s, #0x7f
**	ret
*/
TEST_UNIFORM_Z (and_127_s32_x, svint32_t,
		z0 = svand_n_s32_x (p0, z0, 127),
		z0 = svand_x (p0, z0, 127))

/*
** and_128_s32_x:
**	and	z0\.s, z0\.s, #0x80
**	ret
*/
TEST_UNIFORM_Z (and_128_s32_x, svint32_t,
		z0 = svand_n_s32_x (p0, z0, 128),
		z0 = svand_x (p0, z0, 128))

/*
** and_255_s32_x:
**	and	z0\.s, z0\.s, #0xff
**	ret
*/
TEST_UNIFORM_Z (and_255_s32_x, svint32_t,
		z0 = svand_n_s32_x (p0, z0, 255),
		z0 = svand_x (p0, z0, 255))

/*
** and_256_s32_x:
**	and	z0\.s, z0\.s, #0x100
**	ret
*/
TEST_UNIFORM_Z (and_256_s32_x, svint32_t,
		z0 = svand_n_s32_x (p0, z0, 256),
		z0 = svand_x (p0, z0, 256))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (and_257_s32_x, svint32_t,
		z0 = svand_n_s32_x (p0, z0, 257),
		z0 = svand_x (p0, z0, 257))

/*
** and_512_s32_x:
**	and	z0\.s, z0\.s, #0x200
**	ret
*/
TEST_UNIFORM_Z (and_512_s32_x, svint32_t,
		z0 = svand_n_s32_x (p0, z0, 512),
		z0 = svand_x (p0, z0, 512))

/*
** and_65280_s32_x:
**	and	z0\.s, z0\.s, #0xff00
**	ret
*/
TEST_UNIFORM_Z (and_65280_s32_x, svint32_t,
		z0 = svand_n_s32_x (p0, z0, 0xff00),
		z0 = svand_x (p0, z0, 0xff00))

/*
** and_m127_s32_x:
**	and	z0\.s, z0\.s, #0xffffff81
**	ret
*/
TEST_UNIFORM_Z (and_m127_s32_x, svint32_t,
		z0 = svand_n_s32_x (p0, z0, -127),
		z0 = svand_x (p0, z0, -127))

/*
** and_m128_s32_x:
**	and	z0\.s, z0\.s, #0xffffff80
**	ret
*/
TEST_UNIFORM_Z (and_m128_s32_x, svint32_t,
		z0 = svand_n_s32_x (p0, z0, -128),
		z0 = svand_x (p0, z0, -128))

/*
** and_m255_s32_x:
**	and	z0\.s, z0\.s, #0xffffff01
**	ret
*/
TEST_UNIFORM_Z (and_m255_s32_x, svint32_t,
		z0 = svand_n_s32_x (p0, z0, -255),
		z0 = svand_x (p0, z0, -255))

/*
** and_m256_s32_x:
**	and	z0\.s, z0\.s, #0xffffff00
**	ret
*/
TEST_UNIFORM_Z (and_m256_s32_x, svint32_t,
		z0 = svand_n_s32_x (p0, z0, -256),
		z0 = svand_x (p0, z0, -256))

/*
** and_m257_s32_x:
**	and	z0\.s, z0\.s, #0xfffffeff
**	ret
*/
TEST_UNIFORM_Z (and_m257_s32_x, svint32_t,
		z0 = svand_n_s32_x (p0, z0, -257),
		z0 = svand_x (p0, z0, -257))

/*
** and_m512_s32_x:
**	and	z0\.s, z0\.s, #0xfffffe00
**	ret
*/
TEST_UNIFORM_Z (and_m512_s32_x, svint32_t,
		z0 = svand_n_s32_x (p0, z0, -512),
		z0 = svand_x (p0, z0, -512))

/*
** and_m32768_s32_x:
**	and	z0\.s, z0\.s, #0xffff8000
**	ret
*/
TEST_UNIFORM_Z (and_m32768_s32_x, svint32_t,
		z0 = svand_n_s32_x (p0, z0, -0x8000),
		z0 = svand_x (p0, z0, -0x8000))

/*
** and_5_s32_x:
**	mov	(z[0-9]+)\.s, #5
**	and	z0\.d, (z0\.d, \1\.d|\1\.d, z0\.d)
**	ret
*/
TEST_UNIFORM_Z (and_5_s32_x, svint32_t,
		z0 = svand_n_s32_x (p0, z0, 5),
		z0 = svand_x (p0, z0, 5))
