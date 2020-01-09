/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qadd_s16_tied1:
**	sqadd	z0\.h, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_UNIFORM_Z (qadd_s16_tied1, svint16_t,
		z0 = svqadd_s16 (z0, z1),
		z0 = svqadd (z0, z1))

/*
** qadd_s16_tied2:
**	sqadd	z0\.h, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_UNIFORM_Z (qadd_s16_tied2, svint16_t,
		z0 = svqadd_s16 (z1, z0),
		z0 = svqadd (z1, z0))

/*
** qadd_s16_untied:
**	sqadd	z0\.h, (z1\.h, z2\.h|z2\.h, z1\.h)
**	ret
*/
TEST_UNIFORM_Z (qadd_s16_untied, svint16_t,
		z0 = svqadd_s16 (z1, z2),
		z0 = svqadd (z1, z2))

/*
** qadd_w0_s16_tied1:
**	mov	(z[0-9]+\.h), w0
**	sqadd	z0\.h, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_UNIFORM_ZX (qadd_w0_s16_tied1, svint16_t, int16_t,
		 z0 = svqadd_n_s16 (z0, x0),
		 z0 = svqadd (z0, x0))

/*
** qadd_w0_s16_untied:
**	mov	(z[0-9]+\.h), w0
**	sqadd	z0\.h, (z1\.h, \1|\1, z1\.h)
**	ret
*/
TEST_UNIFORM_ZX (qadd_w0_s16_untied, svint16_t, int16_t,
		 z0 = svqadd_n_s16 (z1, x0),
		 z0 = svqadd (z1, x0))

/*
** qadd_1_s16_tied1:
**	sqadd	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qadd_1_s16_tied1, svint16_t,
		z0 = svqadd_n_s16 (z0, 1),
		z0 = svqadd (z0, 1))

/*
** qadd_1_s16_untied:
**	movprfx	z0, z1
**	sqadd	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qadd_1_s16_untied, svint16_t,
		z0 = svqadd_n_s16 (z1, 1),
		z0 = svqadd (z1, 1))

/*
** qadd_127_s16:
**	sqadd	z0\.h, z0\.h, #127
**	ret
*/
TEST_UNIFORM_Z (qadd_127_s16, svint16_t,
		z0 = svqadd_n_s16 (z0, 127),
		z0 = svqadd (z0, 127))

/*
** qadd_128_s16:
**	sqadd	z0\.h, z0\.h, #128
**	ret
*/
TEST_UNIFORM_Z (qadd_128_s16, svint16_t,
		z0 = svqadd_n_s16 (z0, 128),
		z0 = svqadd (z0, 128))

/*
** qadd_255_s16:
**	sqadd	z0\.h, z0\.h, #255
**	ret
*/
TEST_UNIFORM_Z (qadd_255_s16, svint16_t,
		z0 = svqadd_n_s16 (z0, 255),
		z0 = svqadd (z0, 255))

/*
** qadd_m1_s16:
**	sqsub	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qadd_m1_s16, svint16_t,
		z0 = svqadd_n_s16 (z0, -1),
		z0 = svqadd (z0, -1))

/*
** qadd_m127_s16:
**	sqsub	z0\.h, z0\.h, #127
**	ret
*/
TEST_UNIFORM_Z (qadd_m127_s16, svint16_t,
		z0 = svqadd_n_s16 (z0, -127),
		z0 = svqadd (z0, -127))

/*
** qadd_m128_s16:
**	sqsub	z0\.h, z0\.h, #128
**	ret
*/
TEST_UNIFORM_Z (qadd_m128_s16, svint16_t,
		z0 = svqadd_n_s16 (z0, -128),
		z0 = svqadd (z0, -128))

/*
** qadd_s16_m_tied1:
**	sqadd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (qadd_s16_m_tied1, svint16_t,
		z0 = svqadd_s16_m (p0, z0, z1),
		z0 = svqadd_m (p0, z0, z1))

/*
** qadd_s16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sqadd	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (qadd_s16_m_tied2, svint16_t,
		z0 = svqadd_s16_m (p0, z1, z0),
		z0 = svqadd_m (p0, z1, z0))

/*
** qadd_s16_m_untied:
**	movprfx	z0, z1
**	sqadd	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (qadd_s16_m_untied, svint16_t,
		z0 = svqadd_s16_m (p0, z1, z2),
		z0 = svqadd_m (p0, z1, z2))

/*
** qadd_w0_s16_m_tied1:
**	mov	(z[0-9]+\.h), w0
**	sqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qadd_w0_s16_m_tied1, svint16_t, int16_t,
		 z0 = svqadd_n_s16_m (p0, z0, x0),
		 z0 = svqadd_m (p0, z0, x0))

/*
** qadd_w0_s16_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	sqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qadd_w0_s16_m_untied, svint16_t, int16_t,
		 z0 = svqadd_n_s16_m (p0, z1, x0),
		 z0 = svqadd_m (p0, z1, x0))

/*
** qadd_1_s16_m_tied1:
**	mov	(z[0-9]+\.h), #1
**	sqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_1_s16_m_tied1, svint16_t,
		z0 = svqadd_n_s16_m (p0, z0, 1),
		z0 = svqadd_m (p0, z0, 1))

/*
** qadd_1_s16_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.h), #1
**	movprfx	z0, z1
**	sqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_1_s16_m_untied, svint16_t,
		z0 = svqadd_n_s16_m (p0, z1, 1),
		z0 = svqadd_m (p0, z1, 1))

/*
** qadd_127_s16_m:
**	mov	(z[0-9]+\.h), #127
**	sqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_127_s16_m, svint16_t,
		z0 = svqadd_n_s16_m (p0, z0, 127),
		z0 = svqadd_m (p0, z0, 127))

/*
** qadd_128_s16_m:
**	mov	(z[0-9]+\.h), #128
**	sqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_128_s16_m, svint16_t,
		z0 = svqadd_n_s16_m (p0, z0, 128),
		z0 = svqadd_m (p0, z0, 128))

/*
** qadd_255_s16_m:
**	mov	(z[0-9]+\.h), #255
**	sqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_255_s16_m, svint16_t,
		z0 = svqadd_n_s16_m (p0, z0, 255),
		z0 = svqadd_m (p0, z0, 255))

/*
** qadd_m1_s16_m:
**	mov	(z[0-9]+)\.b, #-1
**	sqadd	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (qadd_m1_s16_m, svint16_t,
		z0 = svqadd_n_s16_m (p0, z0, -1),
		z0 = svqadd_m (p0, z0, -1))

/*
** qadd_m127_s16_m:
**	mov	(z[0-9]+\.h), #-127
**	sqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_m127_s16_m, svint16_t,
		z0 = svqadd_n_s16_m (p0, z0, -127),
		z0 = svqadd_m (p0, z0, -127))

/*
** qadd_m128_s16_m:
**	mov	(z[0-9]+\.h), #-128
**	sqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_m128_s16_m, svint16_t,
		z0 = svqadd_n_s16_m (p0, z0, -128),
		z0 = svqadd_m (p0, z0, -128))

/*
** qadd_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	sqadd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (qadd_s16_z_tied1, svint16_t,
		z0 = svqadd_s16_z (p0, z0, z1),
		z0 = svqadd_z (p0, z0, z1))

/*
** qadd_s16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	sqadd	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (qadd_s16_z_tied2, svint16_t,
		z0 = svqadd_s16_z (p0, z1, z0),
		z0 = svqadd_z (p0, z1, z0))

/*
** qadd_s16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	sqadd	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0\.h, p0/z, z2\.h
**	sqadd	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (qadd_s16_z_untied, svint16_t,
		z0 = svqadd_s16_z (p0, z1, z2),
		z0 = svqadd_z (p0, z1, z2))

/*
** qadd_w0_s16_z_tied1:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	sqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (qadd_w0_s16_z_tied1, svint16_t, int16_t,
		 z0 = svqadd_n_s16_z (p0, z0, x0),
		 z0 = svqadd_z (p0, z0, x0))

/*
** qadd_w0_s16_z_untied:
**	mov	(z[0-9]+\.h), w0
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	sqadd	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	sqadd	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZX (qadd_w0_s16_z_untied, svint16_t, int16_t,
		 z0 = svqadd_n_s16_z (p0, z1, x0),
		 z0 = svqadd_z (p0, z1, x0))

/*
** qadd_1_s16_z_tied1:
**	mov	(z[0-9]+\.h), #1
**	movprfx	z0\.h, p0/z, z0\.h
**	sqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_1_s16_z_tied1, svint16_t,
		z0 = svqadd_n_s16_z (p0, z0, 1),
		z0 = svqadd_z (p0, z0, 1))

/*
** qadd_1_s16_z_untied:
**	mov	(z[0-9]+\.h), #1
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	sqadd	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	sqadd	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (qadd_1_s16_z_untied, svint16_t,
		z0 = svqadd_n_s16_z (p0, z1, 1),
		z0 = svqadd_z (p0, z1, 1))

/*
** qadd_127_s16_z:
**	mov	(z[0-9]+\.h), #127
**	movprfx	z0\.h, p0/z, z0\.h
**	sqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_127_s16_z, svint16_t,
		z0 = svqadd_n_s16_z (p0, z0, 127),
		z0 = svqadd_z (p0, z0, 127))

/*
** qadd_128_s16_z:
**	mov	(z[0-9]+\.h), #128
**	movprfx	z0\.h, p0/z, z0\.h
**	sqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_128_s16_z, svint16_t,
		z0 = svqadd_n_s16_z (p0, z0, 128),
		z0 = svqadd_z (p0, z0, 128))

/*
** qadd_255_s16_z:
**	mov	(z[0-9]+\.h), #255
**	movprfx	z0\.h, p0/z, z0\.h
**	sqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_255_s16_z, svint16_t,
		z0 = svqadd_n_s16_z (p0, z0, 255),
		z0 = svqadd_z (p0, z0, 255))

/*
** qadd_m1_s16_z:
**	mov	(z[0-9]+)\.b, #-1
**	movprfx	z0\.h, p0/z, z0\.h
**	sqadd	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (qadd_m1_s16_z, svint16_t,
		z0 = svqadd_n_s16_z (p0, z0, -1),
		z0 = svqadd_z (p0, z0, -1))

/*
** qadd_m127_s16_z:
**	mov	(z[0-9]+\.h), #-127
**	movprfx	z0\.h, p0/z, z0\.h
**	sqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_m127_s16_z, svint16_t,
		z0 = svqadd_n_s16_z (p0, z0, -127),
		z0 = svqadd_z (p0, z0, -127))

/*
** qadd_m128_s16_z:
**	mov	(z[0-9]+\.h), #-128
**	movprfx	z0\.h, p0/z, z0\.h
**	sqadd	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (qadd_m128_s16_z, svint16_t,
		z0 = svqadd_n_s16_z (p0, z0, -128),
		z0 = svqadd_z (p0, z0, -128))

/*
** qadd_s16_x_tied1:
**	sqadd	z0\.h, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_UNIFORM_Z (qadd_s16_x_tied1, svint16_t,
		z0 = svqadd_s16_x (p0, z0, z1),
		z0 = svqadd_x (p0, z0, z1))

/*
** qadd_s16_x_tied2:
**	sqadd	z0\.h, (z0\.h, z1\.h|z1\.h, z0\.h)
**	ret
*/
TEST_UNIFORM_Z (qadd_s16_x_tied2, svint16_t,
		z0 = svqadd_s16_x (p0, z1, z0),
		z0 = svqadd_x (p0, z1, z0))

/*
** qadd_s16_x_untied:
**	sqadd	z0\.h, (z1\.h, z2\.h|z2\.h, z1\.h)
**	ret
*/
TEST_UNIFORM_Z (qadd_s16_x_untied, svint16_t,
		z0 = svqadd_s16_x (p0, z1, z2),
		z0 = svqadd_x (p0, z1, z2))

/*
** qadd_w0_s16_x_tied1:
**	mov	(z[0-9]+\.h), w0
**	sqadd	z0\.h, (z0\.h, \1|\1, z0\.h)
**	ret
*/
TEST_UNIFORM_ZX (qadd_w0_s16_x_tied1, svint16_t, int16_t,
		 z0 = svqadd_n_s16_x (p0, z0, x0),
		 z0 = svqadd_x (p0, z0, x0))

/*
** qadd_w0_s16_x_untied:
**	mov	(z[0-9]+\.h), w0
**	sqadd	z0\.h, (z1\.h, \1|\1, z1\.h)
**	ret
*/
TEST_UNIFORM_ZX (qadd_w0_s16_x_untied, svint16_t, int16_t,
		 z0 = svqadd_n_s16_x (p0, z1, x0),
		 z0 = svqadd_x (p0, z1, x0))

/*
** qadd_1_s16_x_tied1:
**	sqadd	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qadd_1_s16_x_tied1, svint16_t,
		z0 = svqadd_n_s16_x (p0, z0, 1),
		z0 = svqadd_x (p0, z0, 1))

/*
** qadd_1_s16_x_untied:
**	movprfx	z0, z1
**	sqadd	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qadd_1_s16_x_untied, svint16_t,
		z0 = svqadd_n_s16_x (p0, z1, 1),
		z0 = svqadd_x (p0, z1, 1))

/*
** qadd_127_s16_x:
**	sqadd	z0\.h, z0\.h, #127
**	ret
*/
TEST_UNIFORM_Z (qadd_127_s16_x, svint16_t,
		z0 = svqadd_n_s16_x (p0, z0, 127),
		z0 = svqadd_x (p0, z0, 127))

/*
** qadd_128_s16_x:
**	sqadd	z0\.h, z0\.h, #128
**	ret
*/
TEST_UNIFORM_Z (qadd_128_s16_x, svint16_t,
		z0 = svqadd_n_s16_x (p0, z0, 128),
		z0 = svqadd_x (p0, z0, 128))

/*
** qadd_255_s16_x:
**	sqadd	z0\.h, z0\.h, #255
**	ret
*/
TEST_UNIFORM_Z (qadd_255_s16_x, svint16_t,
		z0 = svqadd_n_s16_x (p0, z0, 255),
		z0 = svqadd_x (p0, z0, 255))

/*
** qadd_m1_s16_x:
**	sqsub	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (qadd_m1_s16_x, svint16_t,
		z0 = svqadd_n_s16_x (p0, z0, -1),
		z0 = svqadd_x (p0, z0, -1))

/*
** qadd_m127_s16_x:
**	sqsub	z0\.h, z0\.h, #127
**	ret
*/
TEST_UNIFORM_Z (qadd_m127_s16_x, svint16_t,
		z0 = svqadd_n_s16_x (p0, z0, -127),
		z0 = svqadd_x (p0, z0, -127))

/*
** qadd_m128_s16_x:
**	sqsub	z0\.h, z0\.h, #128
**	ret
*/
TEST_UNIFORM_Z (qadd_m128_s16_x, svint16_t,
		z0 = svqadd_n_s16_x (p0, z0, -128),
		z0 = svqadd_x (p0, z0, -128))
