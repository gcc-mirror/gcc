/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** subr_u32_m_tied1:
**	subr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (subr_u32_m_tied1, svuint32_t,
		z0 = svsubr_u32_m (p0, z0, z1),
		z0 = svsubr_m (p0, z0, z1))

/*
** subr_u32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	subr	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (subr_u32_m_tied2, svuint32_t,
		z0 = svsubr_u32_m (p0, z1, z0),
		z0 = svsubr_m (p0, z1, z0))

/*
** subr_u32_m_untied:
**	movprfx	z0, z1
**	subr	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (subr_u32_m_untied, svuint32_t,
		z0 = svsubr_u32_m (p0, z1, z2),
		z0 = svsubr_m (p0, z1, z2))

/*
** subr_w0_u32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	subr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (subr_w0_u32_m_tied1, svuint32_t, uint32_t,
		 z0 = svsubr_n_u32_m (p0, z0, x0),
		 z0 = svsubr_m (p0, z0, x0))

/*
** subr_w0_u32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	subr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (subr_w0_u32_m_untied, svuint32_t, uint32_t,
		 z0 = svsubr_n_u32_m (p0, z1, x0),
		 z0 = svsubr_m (p0, z1, x0))

/*
** subr_1_u32_m_tied1:
**	mov	(z[0-9]+\.s), #1
**	subr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (subr_1_u32_m_tied1, svuint32_t,
		z0 = svsubr_n_u32_m (p0, z0, 1),
		z0 = svsubr_m (p0, z0, 1))

/*
** subr_1_u32_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0, z1
**	subr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (subr_1_u32_m_untied, svuint32_t,
		z0 = svsubr_n_u32_m (p0, z1, 1),
		z0 = svsubr_m (p0, z1, 1))

/*
** subr_m2_u32_m:
**	mov	(z[0-9]+\.s), #-2
**	subr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (subr_m2_u32_m, svuint32_t,
		z0 = svsubr_n_u32_m (p0, z0, -2),
		z0 = svsubr_m (p0, z0, -2))

/*
** subr_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	subr	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (subr_u32_z_tied1, svuint32_t,
		z0 = svsubr_u32_z (p0, z0, z1),
		z0 = svsubr_z (p0, z0, z1))

/*
** subr_u32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	sub	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (subr_u32_z_tied2, svuint32_t,
		z0 = svsubr_u32_z (p0, z1, z0),
		z0 = svsubr_z (p0, z1, z0))

/*
** subr_u32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	subr	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	sub	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (subr_u32_z_untied, svuint32_t,
		z0 = svsubr_u32_z (p0, z1, z2),
		z0 = svsubr_z (p0, z1, z2))

/*
** subr_w0_u32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	subr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (subr_w0_u32_z_tied1, svuint32_t, uint32_t,
		 z0 = svsubr_n_u32_z (p0, z0, x0),
		 z0 = svsubr_z (p0, z0, x0))

/*
** subr_w0_u32_z_untied:
**	mov	(z[0-9]+\.s), w0
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	subr	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	sub	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZX (subr_w0_u32_z_untied, svuint32_t, uint32_t,
		 z0 = svsubr_n_u32_z (p0, z1, x0),
		 z0 = svsubr_z (p0, z1, x0))

/*
** subr_1_u32_z_tied1:
**	mov	(z[0-9]+\.s), #1
**	movprfx	z0\.s, p0/z, z0\.s
**	subr	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (subr_1_u32_z_tied1, svuint32_t,
		z0 = svsubr_n_u32_z (p0, z0, 1),
		z0 = svsubr_z (p0, z0, 1))

/*
** subr_1_u32_z_untied:
**	mov	(z[0-9]+\.s), #1
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	subr	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	sub	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (subr_1_u32_z_untied, svuint32_t,
		z0 = svsubr_n_u32_z (p0, z1, 1),
		z0 = svsubr_z (p0, z1, 1))

/*
** subr_u32_x_tied1:
**	sub	z0\.s, z1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (subr_u32_x_tied1, svuint32_t,
		z0 = svsubr_u32_x (p0, z0, z1),
		z0 = svsubr_x (p0, z0, z1))

/*
** subr_u32_x_tied2:
**	sub	z0\.s, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (subr_u32_x_tied2, svuint32_t,
		z0 = svsubr_u32_x (p0, z1, z0),
		z0 = svsubr_x (p0, z1, z0))

/*
** subr_u32_x_untied:
**	sub	z0\.s, z2\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (subr_u32_x_untied, svuint32_t,
		z0 = svsubr_u32_x (p0, z1, z2),
		z0 = svsubr_x (p0, z1, z2))

/*
** subr_w0_u32_x_tied1:
**	mov	(z[0-9]+\.s), w0
**	sub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_ZX (subr_w0_u32_x_tied1, svuint32_t, uint32_t,
		 z0 = svsubr_n_u32_x (p0, z0, x0),
		 z0 = svsubr_x (p0, z0, x0))

/*
** subr_w0_u32_x_untied:
**	mov	(z[0-9]+\.s), w0
**	sub	z0\.s, \1, z1\.s
**	ret
*/
TEST_UNIFORM_ZX (subr_w0_u32_x_untied, svuint32_t, uint32_t,
		 z0 = svsubr_n_u32_x (p0, z1, x0),
		 z0 = svsubr_x (p0, z1, x0))

/*
** subr_1_u32_x_tied1:
**	subr	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (subr_1_u32_x_tied1, svuint32_t,
		z0 = svsubr_n_u32_x (p0, z0, 1),
		z0 = svsubr_x (p0, z0, 1))

/*
** subr_1_u32_x_untied:
**	movprfx	z0, z1
**	subr	z0\.s, z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (subr_1_u32_x_untied, svuint32_t,
		z0 = svsubr_n_u32_x (p0, z1, 1),
		z0 = svsubr_x (p0, z1, 1))

/*
** subr_127_u32_x:
**	subr	z0\.s, z0\.s, #127
**	ret
*/
TEST_UNIFORM_Z (subr_127_u32_x, svuint32_t,
		z0 = svsubr_n_u32_x (p0, z0, 127),
		z0 = svsubr_x (p0, z0, 127))

/*
** subr_128_u32_x:
**	subr	z0\.s, z0\.s, #128
**	ret
*/
TEST_UNIFORM_Z (subr_128_u32_x, svuint32_t,
		z0 = svsubr_n_u32_x (p0, z0, 128),
		z0 = svsubr_x (p0, z0, 128))

/*
** subr_255_u32_x:
**	subr	z0\.s, z0\.s, #255
**	ret
*/
TEST_UNIFORM_Z (subr_255_u32_x, svuint32_t,
		z0 = svsubr_n_u32_x (p0, z0, 255),
		z0 = svsubr_x (p0, z0, 255))

/*
** subr_256_u32_x:
**	subr	z0\.s, z0\.s, #256
**	ret
*/
TEST_UNIFORM_Z (subr_256_u32_x, svuint32_t,
		z0 = svsubr_n_u32_x (p0, z0, 256),
		z0 = svsubr_x (p0, z0, 256))

/*
** subr_511_u32_x:
**	mov	(z[0-9]+\.s), #511
**	sub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (subr_511_u32_x, svuint32_t,
		z0 = svsubr_n_u32_x (p0, z0, 511),
		z0 = svsubr_x (p0, z0, 511))

/*
** subr_512_u32_x:
**	subr	z0\.s, z0\.s, #512
**	ret
*/
TEST_UNIFORM_Z (subr_512_u32_x, svuint32_t,
		z0 = svsubr_n_u32_x (p0, z0, 512),
		z0 = svsubr_x (p0, z0, 512))

/*
** subr_65280_u32_x:
**	subr	z0\.s, z0\.s, #65280
**	ret
*/
TEST_UNIFORM_Z (subr_65280_u32_x, svuint32_t,
		z0 = svsubr_n_u32_x (p0, z0, 0xff00),
		z0 = svsubr_x (p0, z0, 0xff00))

/*
** subr_65535_u32_x:
**	mov	(z[0-9]+\.s), #65535
**	sub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (subr_65535_u32_x, svuint32_t,
		z0 = svsubr_n_u32_x (p0, z0, 65535),
		z0 = svsubr_x (p0, z0, 65535))

/*
** subr_65536_u32_x:
**	mov	(z[0-9]+\.s), #65536
**	sub	z0\.s, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (subr_65536_u32_x, svuint32_t,
		z0 = svsubr_n_u32_x (p0, z0, 65536),
		z0 = svsubr_x (p0, z0, 65536))

/*
** subr_m1_u32_x_tied1:
**	mov	(z[0-9]+)\.b, #-1
**	sub	z0\.s, \1\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (subr_m1_u32_x_tied1, svuint32_t,
		z0 = svsubr_n_u32_x (p0, z0, -1),
		z0 = svsubr_x (p0, z0, -1))

/*
** subr_m1_u32_x_untied:
**	mov	(z[0-9]+)\.b, #-1
**	sub	z0\.s, \1\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (subr_m1_u32_x_untied, svuint32_t,
		z0 = svsubr_n_u32_x (p0, z1, -1),
		z0 = svsubr_x (p0, z1, -1))
