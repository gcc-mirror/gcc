/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** subr_s16_m_tied1:
**	subr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (subr_s16_m_tied1, svint16_t,
		z0 = svsubr_s16_m (p0, z0, z1),
		z0 = svsubr_m (p0, z0, z1))

/*
** subr_s16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	subr	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (subr_s16_m_tied2, svint16_t,
		z0 = svsubr_s16_m (p0, z1, z0),
		z0 = svsubr_m (p0, z1, z0))

/*
** subr_s16_m_untied:
**	movprfx	z0, z1
**	subr	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (subr_s16_m_untied, svint16_t,
		z0 = svsubr_s16_m (p0, z1, z2),
		z0 = svsubr_m (p0, z1, z2))

/*
** subr_w0_s16_m_tied1:
**	mov	(z[0-9]+\.h), w0
**	subr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (subr_w0_s16_m_tied1, svint16_t, int16_t,
		 z0 = svsubr_n_s16_m (p0, z0, x0),
		 z0 = svsubr_m (p0, z0, x0))

/*
** subr_w0_s16_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	subr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (subr_w0_s16_m_untied, svint16_t, int16_t,
		 z0 = svsubr_n_s16_m (p0, z1, x0),
		 z0 = svsubr_m (p0, z1, x0))

/*
** subr_1_s16_m_tied1:
**	mov	(z[0-9]+\.h), #1
**	subr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (subr_1_s16_m_tied1, svint16_t,
		z0 = svsubr_n_s16_m (p0, z0, 1),
		z0 = svsubr_m (p0, z0, 1))

/*
** subr_1_s16_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.h), #1
**	movprfx	z0, z1
**	subr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (subr_1_s16_m_untied, svint16_t,
		z0 = svsubr_n_s16_m (p0, z1, 1),
		z0 = svsubr_m (p0, z1, 1))

/*
** subr_m2_s16_m:
**	mov	(z[0-9]+\.h), #-2
**	subr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (subr_m2_s16_m, svint16_t,
		z0 = svsubr_n_s16_m (p0, z0, -2),
		z0 = svsubr_m (p0, z0, -2))

/*
** subr_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	subr	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (subr_s16_z_tied1, svint16_t,
		z0 = svsubr_s16_z (p0, z0, z1),
		z0 = svsubr_z (p0, z0, z1))

/*
** subr_s16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	sub	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (subr_s16_z_tied2, svint16_t,
		z0 = svsubr_s16_z (p0, z1, z0),
		z0 = svsubr_z (p0, z1, z0))

/*
** subr_s16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	subr	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0\.h, p0/z, z2\.h
**	sub	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (subr_s16_z_untied, svint16_t,
		z0 = svsubr_s16_z (p0, z1, z2),
		z0 = svsubr_z (p0, z1, z2))

/*
** subr_w0_s16_z_tied1:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	subr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (subr_w0_s16_z_tied1, svint16_t, int16_t,
		 z0 = svsubr_n_s16_z (p0, z0, x0),
		 z0 = svsubr_z (p0, z0, x0))

/*
** subr_w0_s16_z_untied:
**	mov	(z[0-9]+\.h), w0
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	subr	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	sub	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZX (subr_w0_s16_z_untied, svint16_t, int16_t,
		 z0 = svsubr_n_s16_z (p0, z1, x0),
		 z0 = svsubr_z (p0, z1, x0))

/*
** subr_1_s16_z_tied1:
**	mov	(z[0-9]+\.h), #1
**	movprfx	z0\.h, p0/z, z0\.h
**	subr	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (subr_1_s16_z_tied1, svint16_t,
		z0 = svsubr_n_s16_z (p0, z0, 1),
		z0 = svsubr_z (p0, z0, 1))

/*
** subr_1_s16_z_untied:
**	mov	(z[0-9]+\.h), #1
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	subr	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	sub	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (subr_1_s16_z_untied, svint16_t,
		z0 = svsubr_n_s16_z (p0, z1, 1),
		z0 = svsubr_z (p0, z1, 1))

/*
** subr_s16_x_tied1:
**	sub	z0\.h, z1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (subr_s16_x_tied1, svint16_t,
		z0 = svsubr_s16_x (p0, z0, z1),
		z0 = svsubr_x (p0, z0, z1))

/*
** subr_s16_x_tied2:
**	sub	z0\.h, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (subr_s16_x_tied2, svint16_t,
		z0 = svsubr_s16_x (p0, z1, z0),
		z0 = svsubr_x (p0, z1, z0))

/*
** subr_s16_x_untied:
**	sub	z0\.h, z2\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (subr_s16_x_untied, svint16_t,
		z0 = svsubr_s16_x (p0, z1, z2),
		z0 = svsubr_x (p0, z1, z2))

/*
** subr_w0_s16_x_tied1:
**	mov	(z[0-9]+\.h), w0
**	sub	z0\.h, \1, z0\.h
**	ret
*/
TEST_UNIFORM_ZX (subr_w0_s16_x_tied1, svint16_t, int16_t,
		 z0 = svsubr_n_s16_x (p0, z0, x0),
		 z0 = svsubr_x (p0, z0, x0))

/*
** subr_w0_s16_x_untied:
**	mov	(z[0-9]+\.h), w0
**	sub	z0\.h, \1, z1\.h
**	ret
*/
TEST_UNIFORM_ZX (subr_w0_s16_x_untied, svint16_t, int16_t,
		 z0 = svsubr_n_s16_x (p0, z1, x0),
		 z0 = svsubr_x (p0, z1, x0))

/*
** subr_1_s16_x_tied1:
**	subr	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (subr_1_s16_x_tied1, svint16_t,
		z0 = svsubr_n_s16_x (p0, z0, 1),
		z0 = svsubr_x (p0, z0, 1))

/*
** subr_1_s16_x_untied:
**	movprfx	z0, z1
**	subr	z0\.h, z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (subr_1_s16_x_untied, svint16_t,
		z0 = svsubr_n_s16_x (p0, z1, 1),
		z0 = svsubr_x (p0, z1, 1))

/*
** subr_127_s16_x:
**	subr	z0\.h, z0\.h, #127
**	ret
*/
TEST_UNIFORM_Z (subr_127_s16_x, svint16_t,
		z0 = svsubr_n_s16_x (p0, z0, 127),
		z0 = svsubr_x (p0, z0, 127))

/*
** subr_128_s16_x:
**	subr	z0\.h, z0\.h, #128
**	ret
*/
TEST_UNIFORM_Z (subr_128_s16_x, svint16_t,
		z0 = svsubr_n_s16_x (p0, z0, 128),
		z0 = svsubr_x (p0, z0, 128))

/*
** subr_255_s16_x:
**	subr	z0\.h, z0\.h, #255
**	ret
*/
TEST_UNIFORM_Z (subr_255_s16_x, svint16_t,
		z0 = svsubr_n_s16_x (p0, z0, 255),
		z0 = svsubr_x (p0, z0, 255))

/*
** subr_256_s16_x:
**	subr	z0\.h, z0\.h, #256
**	ret
*/
TEST_UNIFORM_Z (subr_256_s16_x, svint16_t,
		z0 = svsubr_n_s16_x (p0, z0, 256),
		z0 = svsubr_x (p0, z0, 256))

/*
** subr_257_s16_x:
**	mov	(z[0-9]+)\.b, #1
**	sub	z0\.h, \1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (subr_257_s16_x, svint16_t,
		z0 = svsubr_n_s16_x (p0, z0, 257),
		z0 = svsubr_x (p0, z0, 257))

/*
** subr_512_s16_x:
**	subr	z0\.h, z0\.h, #512
**	ret
*/
TEST_UNIFORM_Z (subr_512_s16_x, svint16_t,
		z0 = svsubr_n_s16_x (p0, z0, 512),
		z0 = svsubr_x (p0, z0, 512))

/*
** subr_65280_s16_x:
**	subr	z0\.h, z0\.h, #65280
**	ret
*/
TEST_UNIFORM_Z (subr_65280_s16_x, svint16_t,
		z0 = svsubr_n_s16_x (p0, z0, 0xff00),
		z0 = svsubr_x (p0, z0, 0xff00))

/*
** subr_m1_s16_x_tied1:
**	mov	(z[0-9]+)\.b, #-1
**	sub	z0\.h, \1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (subr_m1_s16_x_tied1, svint16_t,
		z0 = svsubr_n_s16_x (p0, z0, -1),
		z0 = svsubr_x (p0, z0, -1))

/*
** subr_m1_s16_x_untied:
**	mov	(z[0-9]+)\.b, #-1
**	sub	z0\.h, \1\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (subr_m1_s16_x_untied, svint16_t,
		z0 = svsubr_n_s16_x (p0, z1, -1),
		z0 = svsubr_x (p0, z1, -1))
