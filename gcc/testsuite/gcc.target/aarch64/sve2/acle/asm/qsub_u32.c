/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qsub_u32_tied1:
**	uqsub	z0\.s, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (qsub_u32_tied1, svuint32_t,
		z0 = svqsub_u32 (z0, z1),
		z0 = svqsub (z0, z1))

/*
** qsub_u32_tied2:
**	uqsub	z0\.s, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsub_u32_tied2, svuint32_t,
		z0 = svqsub_u32 (z1, z0),
		z0 = svqsub (z1, z0))

/*
** qsub_u32_untied:
**	uqsub	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (qsub_u32_untied, svuint32_t,
		z0 = svqsub_u32 (z1, z2),
		z0 = svqsub (z1, z2))

/*
** qsub_w0_u32_tied1:
**	mov	(z[0-9]+\.s), w0
**	uqsub	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_u32_tied1, svuint32_t, uint32_t,
		 z0 = svqsub_n_u32 (z0, x0),
		 z0 = svqsub (z0, x0))

/*
** qsub_w0_u32_untied:
**	mov	(z[0-9]+\.s), w0
**	uqsub	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_u32_untied, svuint32_t, uint32_t,
		 z0 = svqsub_n_u32 (z1, x0),
		 z0 = svqsub (z1, x0))

/*
** qsub_1_u32_tied1:
**	uqsub	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u32_tied1, svuint32_t,
		z0 = svqsub_n_u32 (z0, 1),
		z0 = svqsub (z0, 1))

/*
** qsub_1_u32_untied:
**	movprfx	z0, z1
**	uqsub	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u32_untied, svuint32_t,
		z0 = svqsub_n_u32 (z1, 1),
		z0 = svqsub (z1, 1))

/*
** qsub_127_u32:
**	uqsub	z0\.s, z0\.s, #127
**	ret
*/
TEST_UNIFORM_Z (qsub_127_u32, svuint32_t,
		z0 = svqsub_n_u32 (z0, 127),
		z0 = svqsub (z0, 127))

/*
** qsub_128_u32:
**	uqsub	z0\.s, z0\.s, #128
**	ret
*/
TEST_UNIFORM_Z (qsub_128_u32, svuint32_t,
		z0 = svqsub_n_u32 (z0, 128),
		z0 = svqsub (z0, 128))

/*
** qsub_255_u32:
**	uqsub	z0\.s, z0\.s, #255
**	ret
*/
TEST_UNIFORM_Z (qsub_255_u32, svuint32_t,
		z0 = svqsub_n_u32 (z0, 255),
		z0 = svqsub (z0, 255))

/*
** qsub_m1_u32:
**	mov	(z[0-9]+)\.b, #-1
**	uqsub	z0\.s, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (qsub_m1_u32, svuint32_t,
		z0 = svqsub_n_u32 (z0, -1),
		z0 = svqsub (z0, -1))

/*
** qsub_m127_u32:
**	mov	(z[0-9]+\.s), #-127
**	uqsub	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m127_u32, svuint32_t,
		z0 = svqsub_n_u32 (z0, -127),
		z0 = svqsub (z0, -127))

/*
** qsub_m128_u32:
**	mov	(z[0-9]+\.s), #-128
**	uqsub	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m128_u32, svuint32_t,
		z0 = svqsub_n_u32 (z0, -128),
		z0 = svqsub (z0, -128))

/*
** qsub_u32_m_tied1:
**	uqsub	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (qsub_u32_m_tied1, svuint32_t,
		z0 = svqsub_u32_m (p0, z0, z1),
		z0 = svqsub_m (p0, z0, z1))

/*
** qsub_u32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	uqsub	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (qsub_u32_m_tied2, svuint32_t,
		z0 = svqsub_u32_m (p0, z1, z0),
		z0 = svqsub_m (p0, z1, z0))

/*
** qsub_u32_m_untied:
**	movprfx	z0, z1
**	uqsub	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (qsub_u32_m_untied, svuint32_t,
		z0 = svqsub_u32_m (p0, z1, z2),
		z0 = svqsub_m (p0, z1, z2))

/*
** qsub_w0_u32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	uqsub	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_u32_m_tied1, svuint32_t, uint32_t,
		 z0 = svqsub_n_u32_m (p0, z0, x0),
		 z0 = svqsub_m (p0, z0, x0))

/*
** qsub_w0_u32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	uqsub	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_u32_m_untied, svuint32_t, uint32_t,
		 z0 = svqsub_n_u32_m (p0, z1, x0),
		 z0 = svqsub_m (p0, z1, x0))

/*
** qsub_1_u32_m_tied1:
**	mov	(z[0-9]+\.s), #1
**	uqsub	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u32_m_tied1, svuint32_t,
		z0 = svqsub_n_u32_m (p0, z0, 1),
		z0 = svqsub_m (p0, z0, 1))

/*
** qsub_1_u32_m_untied:
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0, z1
**	uqsub	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u32_m_untied, svuint32_t,
		z0 = svqsub_n_u32_m (p0, z1, 1),
		z0 = svqsub_m (p0, z1, 1))

/*
** qsub_127_u32_m:
**	mov	(z[0-9]+\.s), #127
**	uqsub	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_127_u32_m, svuint32_t,
		z0 = svqsub_n_u32_m (p0, z0, 127),
		z0 = svqsub_m (p0, z0, 127))

/*
** qsub_128_u32_m:
**	mov	(z[0-9]+\.s), #128
**	uqsub	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_128_u32_m, svuint32_t,
		z0 = svqsub_n_u32_m (p0, z0, 128),
		z0 = svqsub_m (p0, z0, 128))

/*
** qsub_255_u32_m:
**	mov	(z[0-9]+\.s), #255
**	uqsub	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_255_u32_m, svuint32_t,
		z0 = svqsub_n_u32_m (p0, z0, 255),
		z0 = svqsub_m (p0, z0, 255))

/*
** qsub_m1_u32_m:
**	mov	(z[0-9]+)\.b, #-1
**	uqsub	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (qsub_m1_u32_m, svuint32_t,
		z0 = svqsub_n_u32_m (p0, z0, -1),
		z0 = svqsub_m (p0, z0, -1))

/*
** qsub_m127_u32_m:
**	mov	(z[0-9]+\.s), #-127
**	uqsub	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m127_u32_m, svuint32_t,
		z0 = svqsub_n_u32_m (p0, z0, -127),
		z0 = svqsub_m (p0, z0, -127))

/*
** qsub_m128_u32_m:
**	mov	(z[0-9]+\.s), #-128
**	uqsub	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m128_u32_m, svuint32_t,
		z0 = svqsub_n_u32_m (p0, z0, -128),
		z0 = svqsub_m (p0, z0, -128))

/*
** qsub_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	uqsub	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (qsub_u32_z_tied1, svuint32_t,
		z0 = svqsub_u32_z (p0, z0, z1),
		z0 = svqsub_z (p0, z0, z1))

/*
** qsub_u32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	uqsubr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (qsub_u32_z_tied2, svuint32_t,
		z0 = svqsub_u32_z (p0, z1, z0),
		z0 = svqsub_z (p0, z1, z0))

/*
** qsub_u32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	uqsub	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	uqsubr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (qsub_u32_z_untied, svuint32_t,
		z0 = svqsub_u32_z (p0, z1, z2),
		z0 = svqsub_z (p0, z1, z2))

/*
** qsub_w0_u32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	uqsub	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_u32_z_tied1, svuint32_t, uint32_t,
		 z0 = svqsub_n_u32_z (p0, z0, x0),
		 z0 = svqsub_z (p0, z0, x0))

/*
** qsub_w0_u32_z_untied:
**	mov	(z[0-9]+\.s), w0
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	uqsub	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	uqsubr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_u32_z_untied, svuint32_t, uint32_t,
		 z0 = svqsub_n_u32_z (p0, z1, x0),
		 z0 = svqsub_z (p0, z1, x0))

/*
** qsub_1_u32_z_tied1:
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0\.s, p0/z, z0\.s
**	uqsub	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u32_z_tied1, svuint32_t,
		z0 = svqsub_n_u32_z (p0, z0, 1),
		z0 = svqsub_z (p0, z0, 1))

/*
** qsub_1_u32_z_untied:
**	mov	(z[0-9]+\.s), #1
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	uqsub	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	uqsubr	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u32_z_untied, svuint32_t,
		z0 = svqsub_n_u32_z (p0, z1, 1),
		z0 = svqsub_z (p0, z1, 1))

/*
** qsub_127_u32_z:
**	mov	(z[0-9]+\.s), #127
**	movprfx	z0\.s, p0/z, z0\.s
**	uqsub	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_127_u32_z, svuint32_t,
		z0 = svqsub_n_u32_z (p0, z0, 127),
		z0 = svqsub_z (p0, z0, 127))

/*
** qsub_128_u32_z:
**	mov	(z[0-9]+\.s), #128
**	movprfx	z0\.s, p0/z, z0\.s
**	uqsub	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_128_u32_z, svuint32_t,
		z0 = svqsub_n_u32_z (p0, z0, 128),
		z0 = svqsub_z (p0, z0, 128))

/*
** qsub_255_u32_z:
**	mov	(z[0-9]+\.s), #255
**	movprfx	z0\.s, p0/z, z0\.s
**	uqsub	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_255_u32_z, svuint32_t,
		z0 = svqsub_n_u32_z (p0, z0, 255),
		z0 = svqsub_z (p0, z0, 255))

/*
** qsub_m1_u32_z:
**	mov	(z[0-9]+)\.b, #-1
**	movprfx	z0\.s, p0/z, z0\.s
**	uqsub	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (qsub_m1_u32_z, svuint32_t,
		z0 = svqsub_n_u32_z (p0, z0, -1),
		z0 = svqsub_z (p0, z0, -1))

/*
** qsub_m127_u32_z:
**	mov	(z[0-9]+\.s), #-127
**	movprfx	z0\.s, p0/z, z0\.s
**	uqsub	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m127_u32_z, svuint32_t,
		z0 = svqsub_n_u32_z (p0, z0, -127),
		z0 = svqsub_z (p0, z0, -127))

/*
** qsub_m128_u32_z:
**	mov	(z[0-9]+\.s), #-128
**	movprfx	z0\.s, p0/z, z0\.s
**	uqsub	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m128_u32_z, svuint32_t,
		z0 = svqsub_n_u32_z (p0, z0, -128),
		z0 = svqsub_z (p0, z0, -128))

/*
** qsub_u32_x_tied1:
**	uqsub	z0\.s, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (qsub_u32_x_tied1, svuint32_t,
		z0 = svqsub_u32_x (p0, z0, z1),
		z0 = svqsub_x (p0, z0, z1))

/*
** qsub_u32_x_tied2:
**	uqsub	z0\.s, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (qsub_u32_x_tied2, svuint32_t,
		z0 = svqsub_u32_x (p0, z1, z0),
		z0 = svqsub_x (p0, z1, z0))

/*
** qsub_u32_x_untied:
**	uqsub	z0\.s, z1\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (qsub_u32_x_untied, svuint32_t,
		z0 = svqsub_u32_x (p0, z1, z2),
		z0 = svqsub_x (p0, z1, z2))

/*
** qsub_w0_u32_x_tied1:
**	mov	(z[0-9]+\.s), w0
**	uqsub	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_u32_x_tied1, svuint32_t, uint32_t,
		 z0 = svqsub_n_u32_x (p0, z0, x0),
		 z0 = svqsub_x (p0, z0, x0))

/*
** qsub_w0_u32_x_untied:
**	mov	(z[0-9]+\.s), w0
**	uqsub	z0\.s, z1\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qsub_w0_u32_x_untied, svuint32_t, uint32_t,
		 z0 = svqsub_n_u32_x (p0, z1, x0),
		 z0 = svqsub_x (p0, z1, x0))

/*
** qsub_1_u32_x_tied1:
**	uqsub	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u32_x_tied1, svuint32_t,
		z0 = svqsub_n_u32_x (p0, z0, 1),
		z0 = svqsub_x (p0, z0, 1))

/*
** qsub_1_u32_x_untied:
**	movprfx	z0, z1
**	uqsub	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qsub_1_u32_x_untied, svuint32_t,
		z0 = svqsub_n_u32_x (p0, z1, 1),
		z0 = svqsub_x (p0, z1, 1))

/*
** qsub_127_u32_x:
**	uqsub	z0\.s, z0\.s, #127
**	ret
*/
TEST_UNIFORM_Z (qsub_127_u32_x, svuint32_t,
		z0 = svqsub_n_u32_x (p0, z0, 127),
		z0 = svqsub_x (p0, z0, 127))

/*
** qsub_128_u32_x:
**	uqsub	z0\.s, z0\.s, #128
**	ret
*/
TEST_UNIFORM_Z (qsub_128_u32_x, svuint32_t,
		z0 = svqsub_n_u32_x (p0, z0, 128),
		z0 = svqsub_x (p0, z0, 128))

/*
** qsub_255_u32_x:
**	uqsub	z0\.s, z0\.s, #255
**	ret
*/
TEST_UNIFORM_Z (qsub_255_u32_x, svuint32_t,
		z0 = svqsub_n_u32_x (p0, z0, 255),
		z0 = svqsub_x (p0, z0, 255))

/*
** qsub_m1_u32_x:
**	mov	(z[0-9]+)\.b, #-1
**	uqsub	z0\.s, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (qsub_m1_u32_x, svuint32_t,
		z0 = svqsub_n_u32_x (p0, z0, -1),
		z0 = svqsub_x (p0, z0, -1))

/*
** qsub_m127_u32_x:
**	mov	(z[0-9]+\.s), #-127
**	uqsub	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m127_u32_x, svuint32_t,
		z0 = svqsub_n_u32_x (p0, z0, -127),
		z0 = svqsub_x (p0, z0, -127))

/*
** qsub_m128_u32_x:
**	mov	(z[0-9]+\.s), #-128
**	uqsub	z0\.s, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qsub_m128_u32_x, svuint32_t,
		z0 = svqsub_n_u32_x (p0, z0, -128),
		z0 = svqsub_x (p0, z0, -128))
