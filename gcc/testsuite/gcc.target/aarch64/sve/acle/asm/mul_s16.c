/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** mul_s16_m_tied1:
**	mul	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (mul_s16_m_tied1, svint16_t,
		z0 = svmul_s16_m (p0, z0, z1),
		z0 = svmul_m (p0, z0, z1))

/*
** mul_s16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	mul	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (mul_s16_m_tied2, svint16_t,
		z0 = svmul_s16_m (p0, z1, z0),
		z0 = svmul_m (p0, z1, z0))

/*
** mul_s16_m_untied:
**	movprfx	z0, z1
**	mul	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (mul_s16_m_untied, svint16_t,
		z0 = svmul_s16_m (p0, z1, z2),
		z0 = svmul_m (p0, z1, z2))

/*
** mul_w0_s16_m_tied1:
**	mov	(z[0-9]+\.h), w0
**	mul	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (mul_w0_s16_m_tied1, svint16_t, int16_t,
		 z0 = svmul_n_s16_m (p0, z0, x0),
		 z0 = svmul_m (p0, z0, x0))

/*
** mul_w0_s16_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	mul	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (mul_w0_s16_m_untied, svint16_t, int16_t,
		 z0 = svmul_n_s16_m (p0, z1, x0),
		 z0 = svmul_m (p0, z1, x0))

/*
** mul_2_s16_m_tied1:
**	mov	(z[0-9]+\.h), #2
**	mul	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mul_2_s16_m_tied1, svint16_t,
		z0 = svmul_n_s16_m (p0, z0, 2),
		z0 = svmul_m (p0, z0, 2))

/*
** mul_2_s16_m_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.h), #2
**	movprfx	z0, z1
**	mul	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mul_2_s16_m_untied, svint16_t,
		z0 = svmul_n_s16_m (p0, z1, 2),
		z0 = svmul_m (p0, z1, 2))

/*
** mul_m1_s16_m:
**	mov	(z[0-9]+)\.b, #-1
**	mul	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (mul_m1_s16_m, svint16_t,
		z0 = svmul_n_s16_m (p0, z0, -1),
		z0 = svmul_m (p0, z0, -1))

/*
** mul_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	mul	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (mul_s16_z_tied1, svint16_t,
		z0 = svmul_s16_z (p0, z0, z1),
		z0 = svmul_z (p0, z0, z1))

/*
** mul_s16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	mul	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (mul_s16_z_tied2, svint16_t,
		z0 = svmul_s16_z (p0, z1, z0),
		z0 = svmul_z (p0, z1, z0))

/*
** mul_s16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	mul	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0\.h, p0/z, z2\.h
**	mul	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (mul_s16_z_untied, svint16_t,
		z0 = svmul_s16_z (p0, z1, z2),
		z0 = svmul_z (p0, z1, z2))

/*
** mul_w0_s16_z_tied1:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	mul	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (mul_w0_s16_z_tied1, svint16_t, int16_t,
		 z0 = svmul_n_s16_z (p0, z0, x0),
		 z0 = svmul_z (p0, z0, x0))

/*
** mul_w0_s16_z_untied:
**	mov	(z[0-9]+\.h), w0
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	mul	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	mul	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_ZX (mul_w0_s16_z_untied, svint16_t, int16_t,
		 z0 = svmul_n_s16_z (p0, z1, x0),
		 z0 = svmul_z (p0, z1, x0))

/*
** mul_2_s16_z_tied1:
**	mov	(z[0-9]+\.h), #2
**	movprfx	z0\.h, p0/z, z0\.h
**	mul	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mul_2_s16_z_tied1, svint16_t,
		z0 = svmul_n_s16_z (p0, z0, 2),
		z0 = svmul_z (p0, z0, 2))

/*
** mul_2_s16_z_untied:
**	mov	(z[0-9]+\.h), #2
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	mul	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	mul	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (mul_2_s16_z_untied, svint16_t,
		z0 = svmul_n_s16_z (p0, z1, 2),
		z0 = svmul_z (p0, z1, 2))

/*
** mul_s16_x_tied1:
**	mul	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (mul_s16_x_tied1, svint16_t,
		z0 = svmul_s16_x (p0, z0, z1),
		z0 = svmul_x (p0, z0, z1))

/*
** mul_s16_x_tied2:
**	mul	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (mul_s16_x_tied2, svint16_t,
		z0 = svmul_s16_x (p0, z1, z0),
		z0 = svmul_x (p0, z1, z0))

/*
** mul_s16_x_untied:
** (
**	movprfx	z0, z1
**	mul	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0, z2
**	mul	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (mul_s16_x_untied, svint16_t,
		z0 = svmul_s16_x (p0, z1, z2),
		z0 = svmul_x (p0, z1, z2))

/*
** mul_w0_s16_x_tied1:
**	mov	(z[0-9]+\.h), w0
**	mul	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (mul_w0_s16_x_tied1, svint16_t, int16_t,
		 z0 = svmul_n_s16_x (p0, z0, x0),
		 z0 = svmul_x (p0, z0, x0))

/*
** mul_w0_s16_x_untied:
**	mov	z0\.h, w0
**	mul	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZX (mul_w0_s16_x_untied, svint16_t, int16_t,
		 z0 = svmul_n_s16_x (p0, z1, x0),
		 z0 = svmul_x (p0, z1, x0))

/*
** mul_2_s16_x_tied1:
**	mul	z0\.h, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_2_s16_x_tied1, svint16_t,
		z0 = svmul_n_s16_x (p0, z0, 2),
		z0 = svmul_x (p0, z0, 2))

/*
** mul_2_s16_x_untied:
**	movprfx	z0, z1
**	mul	z0\.h, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_2_s16_x_untied, svint16_t,
		z0 = svmul_n_s16_x (p0, z1, 2),
		z0 = svmul_x (p0, z1, 2))

/*
** mul_127_s16_x:
**	mul	z0\.h, z0\.h, #127
**	ret
*/
TEST_UNIFORM_Z (mul_127_s16_x, svint16_t,
		z0 = svmul_n_s16_x (p0, z0, 127),
		z0 = svmul_x (p0, z0, 127))

/*
** mul_128_s16_x:
**	mov	(z[0-9]+\.h), #128
**	mul	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mul_128_s16_x, svint16_t,
		z0 = svmul_n_s16_x (p0, z0, 128),
		z0 = svmul_x (p0, z0, 128))

/*
** mul_255_s16_x:
**	mov	(z[0-9]+\.h), #255
**	mul	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mul_255_s16_x, svint16_t,
		z0 = svmul_n_s16_x (p0, z0, 255),
		z0 = svmul_x (p0, z0, 255))

/*
** mul_m1_s16_x:
**	mul	z0\.h, z0\.h, #-1
**	ret
*/
TEST_UNIFORM_Z (mul_m1_s16_x, svint16_t,
		z0 = svmul_n_s16_x (p0, z0, -1),
		z0 = svmul_x (p0, z0, -1))

/*
** mul_m127_s16_x:
**	mul	z0\.h, z0\.h, #-127
**	ret
*/
TEST_UNIFORM_Z (mul_m127_s16_x, svint16_t,
		z0 = svmul_n_s16_x (p0, z0, -127),
		z0 = svmul_x (p0, z0, -127))

/*
** mul_m128_s16_x:
**	mul	z0\.h, z0\.h, #-128
**	ret
*/
TEST_UNIFORM_Z (mul_m128_s16_x, svint16_t,
		z0 = svmul_n_s16_x (p0, z0, -128),
		z0 = svmul_x (p0, z0, -128))
