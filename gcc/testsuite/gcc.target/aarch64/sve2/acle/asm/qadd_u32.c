/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qadd_u32_tied1:
**	uqadd	z0\.s, (z0\.s, z1\.s|z1\.s, z0\.s)
**	ret
*/
TEST_UNIFORM_Z (qadd_u32_tied1, svuint32_t,
		z0 = svqadd_u32 (z0, z1),
		z0 = svqadd (z0, z1))

/*
** qadd_u32_tied2:
**	uqadd	z0\.s, (z0\.s, z1\.s|z1\.s, z0\.s)
**	ret
*/
TEST_UNIFORM_Z (qadd_u32_tied2, svuint32_t,
		z0 = svqadd_u32 (z1, z0),
		z0 = svqadd (z1, z0))

/*
** qadd_u32_untied:
**	uqadd	z0\.s, (z1\.s, z2\.s|z2\.s, z1\.s)
**	ret
*/
TEST_UNIFORM_Z (qadd_u32_untied, svuint32_t,
		z0 = svqadd_u32 (z1, z2),
		z0 = svqadd (z1, z2))

/*
** qadd_w0_u32_tied1:
**	mov	(z[0-9]+\.s), w0
**	uqadd	z0\.s, (z0\.s, \1|\1, z0\.s)
**	ret
*/
TEST_UNIFORM_ZX (qadd_w0_u32_tied1, svuint32_t, uint32_t,
		 z0 = svqadd_n_u32 (z0, x0),
		 z0 = svqadd (z0, x0))

/*
** qadd_w0_u32_untied:
**	mov	(z[0-9]+\.s), w0
**	uqadd	z0\.s, (z1\.s, \1|\1, z1\.s)
**	ret
*/
TEST_UNIFORM_ZX (qadd_w0_u32_untied, svuint32_t, uint32_t,
		 z0 = svqadd_n_u32 (z1, x0),
		 z0 = svqadd (z1, x0))

/*
** qadd_1_u32_tied1:
**	uqadd	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qadd_1_u32_tied1, svuint32_t,
		z0 = svqadd_n_u32 (z0, 1),
		z0 = svqadd (z0, 1))

/*
** qadd_1_u32_untied:
**	movprfx	z0, z1
**	uqadd	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qadd_1_u32_untied, svuint32_t,
		z0 = svqadd_n_u32 (z1, 1),
		z0 = svqadd (z1, 1))

/*
** qadd_127_u32:
**	uqadd	z0\.s, z0\.s, #127
**	ret
*/
TEST_UNIFORM_Z (qadd_127_u32, svuint32_t,
		z0 = svqadd_n_u32 (z0, 127),
		z0 = svqadd (z0, 127))

/*
** qadd_128_u32:
**	uqadd	z0\.s, z0\.s, #128
**	ret
*/
TEST_UNIFORM_Z (qadd_128_u32, svuint32_t,
		z0 = svqadd_n_u32 (z0, 128),
		z0 = svqadd (z0, 128))

/*
** qadd_255_u32:
**	uqadd	z0\.s, z0\.s, #255
**	ret
*/
TEST_UNIFORM_Z (qadd_255_u32, svuint32_t,
		z0 = svqadd_n_u32 (z0, 255),
		z0 = svqadd (z0, 255))

/*
** qadd_m1_u32:
**	mov	(z[0-9]+)\.b, #-1
**	uqadd	z0\.s, (z0\.s, \1\.s|\1\.s, z0\.s)
**	ret
*/
TEST_UNIFORM_Z (qadd_m1_u32, svuint32_t,
		z0 = svqadd_n_u32 (z0, -1),
		z0 = svqadd (z0, -1))

/*
** qadd_m127_u32:
**	mov	(z[0-9]+\.s), #-127
**	uqadd	z0\.s, (z0\.s, \1|\1, z0\.s)
**	ret
*/
TEST_UNIFORM_Z (qadd_m127_u32, svuint32_t,
		z0 = svqadd_n_u32 (z0, -127),
		z0 = svqadd (z0, -127))

/*
** qadd_m128_u32:
**	mov	(z[0-9]+\.s), #-128
**	uqadd	z0\.s, (z0\.s, \1|\1, z0\.s)
**	ret
*/
TEST_UNIFORM_Z (qadd_m128_u32, svuint32_t,
		z0 = svqadd_n_u32 (z0, -128),
		z0 = svqadd (z0, -128))

/*
** qadd_u32_m_tied1:
**	uqadd	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (qadd_u32_m_tied1, svuint32_t,
		z0 = svqadd_u32_m (p0, z0, z1),
		z0 = svqadd_m (p0, z0, z1))

/*
** qadd_u32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	uqadd	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (qadd_u32_m_tied2, svuint32_t,
		z0 = svqadd_u32_m (p0, z1, z0),
		z0 = svqadd_m (p0, z1, z0))

/*
** qadd_u32_m_untied:
**	movprfx	z0, z1
**	uqadd	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (qadd_u32_m_untied, svuint32_t,
		z0 = svqadd_u32_m (p0, z1, z2),
		z0 = svqadd_m (p0, z1, z2))

/*
** qadd_w0_u32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	uqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qadd_w0_u32_m_tied1, svuint32_t, uint32_t,
		 z0 = svqadd_n_u32_m (p0, z0, x0),
		 z0 = svqadd_m (p0, z0, x0))

/*
** qadd_w0_u32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	uqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qadd_w0_u32_m_untied, svuint32_t, uint32_t,
		 z0 = svqadd_n_u32_m (p0, z1, x0),
		 z0 = svqadd_m (p0, z1, x0))

/*
** qadd_1_u32_m_tied1:
**	mov	(z[0-9]+\.s), #1
**	uqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_1_u32_m_tied1, svuint32_t,
		z0 = svqadd_n_u32_m (p0, z0, 1),
		z0 = svqadd_m (p0, z0, 1))

/*
** qadd_1_u32_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0, z1
**	uqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_1_u32_m_untied, svuint32_t,
		z0 = svqadd_n_u32_m (p0, z1, 1),
		z0 = svqadd_m (p0, z1, 1))

/*
** qadd_127_u32_m:
**	mov	(z[0-9]+\.s), #127
**	uqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_127_u32_m, svuint32_t,
		z0 = svqadd_n_u32_m (p0, z0, 127),
		z0 = svqadd_m (p0, z0, 127))

/*
** qadd_128_u32_m:
**	mov	(z[0-9]+\.s), #128
**	uqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_128_u32_m, svuint32_t,
		z0 = svqadd_n_u32_m (p0, z0, 128),
		z0 = svqadd_m (p0, z0, 128))

/*
** qadd_255_u32_m:
**	mov	(z[0-9]+\.s), #255
**	uqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_255_u32_m, svuint32_t,
		z0 = svqadd_n_u32_m (p0, z0, 255),
		z0 = svqadd_m (p0, z0, 255))

/*
** qadd_m1_u32_m:
**	mov	(z[0-9]+)\.b, #-1
**	uqadd	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (qadd_m1_u32_m, svuint32_t,
		z0 = svqadd_n_u32_m (p0, z0, -1),
		z0 = svqadd_m (p0, z0, -1))

/*
** qadd_m127_u32_m:
**	mov	(z[0-9]+\.s), #-127
**	uqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_m127_u32_m, svuint32_t,
		z0 = svqadd_n_u32_m (p0, z0, -127),
		z0 = svqadd_m (p0, z0, -127))

/*
** qadd_m128_u32_m:
**	mov	(z[0-9]+\.s), #-128
**	uqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_m128_u32_m, svuint32_t,
		z0 = svqadd_n_u32_m (p0, z0, -128),
		z0 = svqadd_m (p0, z0, -128))

/*
** qadd_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	uqadd	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (qadd_u32_z_tied1, svuint32_t,
		z0 = svqadd_u32_z (p0, z0, z1),
		z0 = svqadd_z (p0, z0, z1))

/*
** qadd_u32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	uqadd	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (qadd_u32_z_tied2, svuint32_t,
		z0 = svqadd_u32_z (p0, z1, z0),
		z0 = svqadd_z (p0, z1, z0))

/*
** qadd_u32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	uqadd	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	uqadd	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (qadd_u32_z_untied, svuint32_t,
		z0 = svqadd_u32_z (p0, z1, z2),
		z0 = svqadd_z (p0, z1, z2))

/*
** qadd_w0_u32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	uqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (qadd_w0_u32_z_tied1, svuint32_t, uint32_t,
		 z0 = svqadd_n_u32_z (p0, z0, x0),
		 z0 = svqadd_z (p0, z0, x0))

/*
** qadd_w0_u32_z_untied:
**	mov	(z[0-9]+\.s), w0
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	uqadd	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	uqadd	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZX (qadd_w0_u32_z_untied, svuint32_t, uint32_t,
		 z0 = svqadd_n_u32_z (p0, z1, x0),
		 z0 = svqadd_z (p0, z1, x0))

/*
** qadd_1_u32_z_tied1:
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0\.s, p0/z, z0\.s
**	uqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_1_u32_z_tied1, svuint32_t,
		z0 = svqadd_n_u32_z (p0, z0, 1),
		z0 = svqadd_z (p0, z0, 1))

/*
** qadd_1_u32_z_untied:
**	mov	(z[0-9]+\.s), #1
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	uqadd	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	uqadd	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (qadd_1_u32_z_untied, svuint32_t,
		z0 = svqadd_n_u32_z (p0, z1, 1),
		z0 = svqadd_z (p0, z1, 1))

/*
** qadd_127_u32_z:
**	mov	(z[0-9]+\.s), #127
**	movprfx	z0\.s, p0/z, z0\.s
**	uqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_127_u32_z, svuint32_t,
		z0 = svqadd_n_u32_z (p0, z0, 127),
		z0 = svqadd_z (p0, z0, 127))

/*
** qadd_128_u32_z:
**	mov	(z[0-9]+\.s), #128
**	movprfx	z0\.s, p0/z, z0\.s
**	uqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_128_u32_z, svuint32_t,
		z0 = svqadd_n_u32_z (p0, z0, 128),
		z0 = svqadd_z (p0, z0, 128))

/*
** qadd_255_u32_z:
**	mov	(z[0-9]+\.s), #255
**	movprfx	z0\.s, p0/z, z0\.s
**	uqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_255_u32_z, svuint32_t,
		z0 = svqadd_n_u32_z (p0, z0, 255),
		z0 = svqadd_z (p0, z0, 255))

/*
** qadd_m1_u32_z:
**	mov	(z[0-9]+)\.b, #-1
**	movprfx	z0\.s, p0/z, z0\.s
**	uqadd	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (qadd_m1_u32_z, svuint32_t,
		z0 = svqadd_n_u32_z (p0, z0, -1),
		z0 = svqadd_z (p0, z0, -1))

/*
** qadd_m127_u32_z:
**	mov	(z[0-9]+\.s), #-127
**	movprfx	z0\.s, p0/z, z0\.s
**	uqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_m127_u32_z, svuint32_t,
		z0 = svqadd_n_u32_z (p0, z0, -127),
		z0 = svqadd_z (p0, z0, -127))

/*
** qadd_m128_u32_z:
**	mov	(z[0-9]+\.s), #-128
**	movprfx	z0\.s, p0/z, z0\.s
**	uqadd	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_m128_u32_z, svuint32_t,
		z0 = svqadd_n_u32_z (p0, z0, -128),
		z0 = svqadd_z (p0, z0, -128))

/*
** qadd_u32_x_tied1:
**	uqadd	z0\.s, (z0\.s, z1\.s|z1\.s, z0\.s)
**	ret
*/
TEST_UNIFORM_Z (qadd_u32_x_tied1, svuint32_t,
		z0 = svqadd_u32_x (p0, z0, z1),
		z0 = svqadd_x (p0, z0, z1))

/*
** qadd_u32_x_tied2:
**	uqadd	z0\.s, (z0\.s, z1\.s|z1\.s, z0\.s)
**	ret
*/
TEST_UNIFORM_Z (qadd_u32_x_tied2, svuint32_t,
		z0 = svqadd_u32_x (p0, z1, z0),
		z0 = svqadd_x (p0, z1, z0))

/*
** qadd_u32_x_untied:
**	uqadd	z0\.s, (z1\.s, z2\.s|z2\.s, z1\.s)
**	ret
*/
TEST_UNIFORM_Z (qadd_u32_x_untied, svuint32_t,
		z0 = svqadd_u32_x (p0, z1, z2),
		z0 = svqadd_x (p0, z1, z2))

/*
** qadd_w0_u32_x_tied1:
**	mov	(z[0-9]+\.s), w0
**	uqadd	z0\.s, (z0\.s, \1|\1, z0\.s)
**	ret
*/
TEST_UNIFORM_ZX (qadd_w0_u32_x_tied1, svuint32_t, uint32_t,
		 z0 = svqadd_n_u32_x (p0, z0, x0),
		 z0 = svqadd_x (p0, z0, x0))

/*
** qadd_w0_u32_x_untied:
**	mov	(z[0-9]+\.s), w0
**	uqadd	z0\.s, (z1\.s, \1|\1, z1\.s)
**	ret
*/
TEST_UNIFORM_ZX (qadd_w0_u32_x_untied, svuint32_t, uint32_t,
		 z0 = svqadd_n_u32_x (p0, z1, x0),
		 z0 = svqadd_x (p0, z1, x0))

/*
** qadd_1_u32_x_tied1:
**	uqadd	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qadd_1_u32_x_tied1, svuint32_t,
		z0 = svqadd_n_u32_x (p0, z0, 1),
		z0 = svqadd_x (p0, z0, 1))

/*
** qadd_1_u32_x_untied:
**	movprfx	z0, z1
**	uqadd	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (qadd_1_u32_x_untied, svuint32_t,
		z0 = svqadd_n_u32_x (p0, z1, 1),
		z0 = svqadd_x (p0, z1, 1))

/*
** qadd_127_u32_x:
**	uqadd	z0\.s, z0\.s, #127
**	ret
*/
TEST_UNIFORM_Z (qadd_127_u32_x, svuint32_t,
		z0 = svqadd_n_u32_x (p0, z0, 127),
		z0 = svqadd_x (p0, z0, 127))

/*
** qadd_128_u32_x:
**	uqadd	z0\.s, z0\.s, #128
**	ret
*/
TEST_UNIFORM_Z (qadd_128_u32_x, svuint32_t,
		z0 = svqadd_n_u32_x (p0, z0, 128),
		z0 = svqadd_x (p0, z0, 128))

/*
** qadd_255_u32_x:
**	uqadd	z0\.s, z0\.s, #255
**	ret
*/
TEST_UNIFORM_Z (qadd_255_u32_x, svuint32_t,
		z0 = svqadd_n_u32_x (p0, z0, 255),
		z0 = svqadd_x (p0, z0, 255))

/*
** qadd_m1_u32_x:
**	mov	(z[0-9]+)\.b, #-1
**	uqadd	z0\.s, (z0\.s, \1\.s|\1\.s, z0\.s)
**	ret
*/
TEST_UNIFORM_Z (qadd_m1_u32_x, svuint32_t,
		z0 = svqadd_n_u32_x (p0, z0, -1),
		z0 = svqadd_x (p0, z0, -1))

/*
** qadd_m127_u32_x:
**	mov	(z[0-9]+\.s), #-127
**	uqadd	z0\.s, (z0\.s, \1|\1, z0\.s)
**	ret
*/
TEST_UNIFORM_Z (qadd_m127_u32_x, svuint32_t,
		z0 = svqadd_n_u32_x (p0, z0, -127),
		z0 = svqadd_x (p0, z0, -127))

/*
** qadd_m128_u32_x:
**	mov	(z[0-9]+\.s), #-128
**	uqadd	z0\.s, (z0\.s, \1|\1, z0\.s)
**	ret
*/
TEST_UNIFORM_Z (qadd_m128_u32_x, svuint32_t,
		z0 = svqadd_n_u32_x (p0, z0, -128),
		z0 = svqadd_x (p0, z0, -128))
