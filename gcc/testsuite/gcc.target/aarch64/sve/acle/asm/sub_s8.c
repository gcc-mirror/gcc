/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** sub_s8_m_tied1:
**	sub	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (sub_s8_m_tied1, svint8_t,
		z0 = svsub_s8_m (p0, z0, z1),
		z0 = svsub_m (p0, z0, z1))

/*
** sub_s8_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	sub	z0\.b, p0/m, z0\.b, \1\.b
**	ret
*/
TEST_UNIFORM_Z (sub_s8_m_tied2, svint8_t,
		z0 = svsub_s8_m (p0, z1, z0),
		z0 = svsub_m (p0, z1, z0))

/*
** sub_s8_m_untied:
**	movprfx	z0, z1
**	sub	z0\.b, p0/m, z0\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (sub_s8_m_untied, svint8_t,
		z0 = svsub_s8_m (p0, z1, z2),
		z0 = svsub_m (p0, z1, z2))

/*
** sub_w0_s8_m_tied1:
**	mov	(z[0-9]+\.b), w0
**	sub	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (sub_w0_s8_m_tied1, svint8_t, int8_t,
		 z0 = svsub_n_s8_m (p0, z0, x0),
		 z0 = svsub_m (p0, z0, x0))

/*
** sub_w0_s8_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0, z1
**	sub	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (sub_w0_s8_m_untied, svint8_t, int8_t,
		 z0 = svsub_n_s8_m (p0, z1, x0),
		 z0 = svsub_m (p0, z1, x0))

/*
** sub_1_s8_m_tied1:
**	mov	(z[0-9]+\.b), #-1
**	add	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (sub_1_s8_m_tied1, svint8_t,
		z0 = svsub_n_s8_m (p0, z0, 1),
		z0 = svsub_m (p0, z0, 1))

/*
** sub_1_s8_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.b), #-1
**	movprfx	z0, z1
**	add	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (sub_1_s8_m_untied, svint8_t,
		z0 = svsub_n_s8_m (p0, z1, 1),
		z0 = svsub_m (p0, z1, 1))

/*
** sub_m1_s8_m:
**	mov	(z[0-9]+\.b), #1
**	add	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (sub_m1_s8_m, svint8_t,
		z0 = svsub_n_s8_m (p0, z0, -1),
		z0 = svsub_m (p0, z0, -1))

/*
** sub_s8_z_tied1:
**	movprfx	z0\.b, p0/z, z0\.b
**	sub	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (sub_s8_z_tied1, svint8_t,
		z0 = svsub_s8_z (p0, z0, z1),
		z0 = svsub_z (p0, z0, z1))

/*
** sub_s8_z_tied2:
**	movprfx	z0\.b, p0/z, z0\.b
**	subr	z0\.b, p0/m, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (sub_s8_z_tied2, svint8_t,
		z0 = svsub_s8_z (p0, z1, z0),
		z0 = svsub_z (p0, z1, z0))

/*
** sub_s8_z_untied:
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	sub	z0\.b, p0/m, z0\.b, z2\.b
** |
**	movprfx	z0\.b, p0/z, z2\.b
**	subr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (sub_s8_z_untied, svint8_t,
		z0 = svsub_s8_z (p0, z1, z2),
		z0 = svsub_z (p0, z1, z2))

/*
** sub_w0_s8_z_tied1:
**	mov	(z[0-9]+\.b), w0
**	movprfx	z0\.b, p0/z, z0\.b
**	sub	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (sub_w0_s8_z_tied1, svint8_t, int8_t,
		 z0 = svsub_n_s8_z (p0, z0, x0),
		 z0 = svsub_z (p0, z0, x0))

/*
** sub_w0_s8_z_untied:
**	mov	(z[0-9]+\.b), w0
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	sub	z0\.b, p0/m, z0\.b, \1
** |
**	movprfx	z0\.b, p0/z, \1
**	subr	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_ZX (sub_w0_s8_z_untied, svint8_t, int8_t,
		 z0 = svsub_n_s8_z (p0, z1, x0),
		 z0 = svsub_z (p0, z1, x0))

/*
** sub_1_s8_z_tied1:
**	mov	(z[0-9]+\.b), #-1
**	movprfx	z0\.b, p0/z, z0\.b
**	add	z0\.b, p0/m, z0\.b, \1
**	ret
*/
TEST_UNIFORM_Z (sub_1_s8_z_tied1, svint8_t,
		z0 = svsub_n_s8_z (p0, z0, 1),
		z0 = svsub_z (p0, z0, 1))

/*
** sub_1_s8_z_untied:
**	mov	(z[0-9]+\.b), #-1
** (
**	movprfx	z0\.b, p0/z, z1\.b
**	add	z0\.b, p0/m, z0\.b, \1
** |
**	movprfx	z0\.b, p0/z, \1
**	add	z0\.b, p0/m, z0\.b, z1\.b
** )
**	ret
*/
TEST_UNIFORM_Z (sub_1_s8_z_untied, svint8_t,
		z0 = svsub_n_s8_z (p0, z1, 1),
		z0 = svsub_z (p0, z1, 1))

/*
** sub_s8_x_tied1:
**	sub	z0\.b, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (sub_s8_x_tied1, svint8_t,
		z0 = svsub_s8_x (p0, z0, z1),
		z0 = svsub_x (p0, z0, z1))

/*
** sub_s8_x_tied2:
**	sub	z0\.b, z1\.b, z0\.b
**	ret
*/
TEST_UNIFORM_Z (sub_s8_x_tied2, svint8_t,
		z0 = svsub_s8_x (p0, z1, z0),
		z0 = svsub_x (p0, z1, z0))

/*
** sub_s8_x_untied:
**	sub	z0\.b, z1\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (sub_s8_x_untied, svint8_t,
		z0 = svsub_s8_x (p0, z1, z2),
		z0 = svsub_x (p0, z1, z2))

/*
** sub_w0_s8_x_tied1:
**	mov	(z[0-9]+\.b), w0
**	sub	z0\.b, z0\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (sub_w0_s8_x_tied1, svint8_t, int8_t,
		 z0 = svsub_n_s8_x (p0, z0, x0),
		 z0 = svsub_x (p0, z0, x0))

/*
** sub_w0_s8_x_untied:
**	mov	(z[0-9]+\.b), w0
**	sub	z0\.b, z1\.b, \1
**	ret
*/
TEST_UNIFORM_ZX (sub_w0_s8_x_untied, svint8_t, int8_t,
		 z0 = svsub_n_s8_x (p0, z1, x0),
		 z0 = svsub_x (p0, z1, x0))

/*
** sub_1_s8_x_tied1:
**	add	z0\.b, z0\.b, #255
**	ret
*/
TEST_UNIFORM_Z (sub_1_s8_x_tied1, svint8_t,
		z0 = svsub_n_s8_x (p0, z0, 1),
		z0 = svsub_x (p0, z0, 1))

/*
** sub_1_s8_x_untied:
**	movprfx	z0, z1
**	add	z0\.b, z0\.b, #255
**	ret
*/
TEST_UNIFORM_Z (sub_1_s8_x_untied, svint8_t,
		z0 = svsub_n_s8_x (p0, z1, 1),
		z0 = svsub_x (p0, z1, 1))

/*
** sub_127_s8_x:
**	add	z0\.b, z0\.b, #129
**	ret
*/
TEST_UNIFORM_Z (sub_127_s8_x, svint8_t,
		z0 = svsub_n_s8_x (p0, z0, 127),
		z0 = svsub_x (p0, z0, 127))

/*
** sub_128_s8_x:
**	add	z0\.b, z0\.b, #128
**	ret
*/
TEST_UNIFORM_Z (sub_128_s8_x, svint8_t,
		z0 = svsub_n_s8_x (p0, z0, 128),
		z0 = svsub_x (p0, z0, 128))

/*
** sub_255_s8_x:
**	add	z0\.b, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (sub_255_s8_x, svint8_t,
		z0 = svsub_n_s8_x (p0, z0, 255),
		z0 = svsub_x (p0, z0, 255))

/*
** sub_m1_s8_x:
**	add	z0\.b, z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (sub_m1_s8_x, svint8_t,
		z0 = svsub_n_s8_x (p0, z0, -1),
		z0 = svsub_x (p0, z0, -1))

/*
** sub_m127_s8_x:
**	add	z0\.b, z0\.b, #127
**	ret
*/
TEST_UNIFORM_Z (sub_m127_s8_x, svint8_t,
		z0 = svsub_n_s8_x (p0, z0, -127),
		z0 = svsub_x (p0, z0, -127))

/*
** sub_m128_s8_x:
**	add	z0\.b, z0\.b, #128
**	ret
*/
TEST_UNIFORM_Z (sub_m128_s8_x, svint8_t,
		z0 = svsub_n_s8_x (p0, z0, -128),
		z0 = svsub_x (p0, z0, -128))
