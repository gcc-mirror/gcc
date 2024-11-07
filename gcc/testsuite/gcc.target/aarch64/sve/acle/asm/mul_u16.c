/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#define MAXPOW 1ULL<<15

/*
** mul_u16_m_tied1:
**	mul	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (mul_u16_m_tied1, svuint16_t,
		z0 = svmul_u16_m (p0, z0, z1),
		z0 = svmul_m (p0, z0, z1))

/*
** mul_u16_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	mul	z0\.h, p0/m, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (mul_u16_m_tied2, svuint16_t,
		z0 = svmul_u16_m (p0, z1, z0),
		z0 = svmul_m (p0, z1, z0))

/*
** mul_u16_m_untied:
**	movprfx	z0, z1
**	mul	z0\.h, p0/m, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (mul_u16_m_untied, svuint16_t,
		z0 = svmul_u16_m (p0, z1, z2),
		z0 = svmul_m (p0, z1, z2))

/*
** mul_w0_u16_m_tied1:
**	mov	(z[0-9]+\.h), w0
**	mul	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (mul_w0_u16_m_tied1, svuint16_t, uint16_t,
		 z0 = svmul_n_u16_m (p0, z0, x0),
		 z0 = svmul_m (p0, z0, x0))

/*
** mul_w0_u16_m_untied:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	mul	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (mul_w0_u16_m_untied, svuint16_t, uint16_t,
		 z0 = svmul_n_u16_m (p0, z1, x0),
		 z0 = svmul_m (p0, z1, x0))

/*
** mul_4dupop1_u16_m_tied2:
**	mov	(z[0-9]+)\.h, #4
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, \1
**	mul	z0\.h, p0/m, z0\.h, \2\.h
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1_u16_m_tied2, svuint16_t,
		z0 = svmul_m (p0, svdup_u16 (4), z0),
		z0 = svmul_m (p0, svdup_u16 (4), z0))

/*
** mul_4dupop1ptrue_u16_m_tied2:
**	lsl	z0\.h, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1ptrue_u16_m_tied2, svuint16_t,
		z0 = svmul_m (svptrue_b16 (), svdup_u16 (4), z0),
		z0 = svmul_m (svptrue_b16 (), svdup_u16 (4), z0))

/*
** mul_4dupop2_u16_m_tied1:
**	lsl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_u16_m_tied1, svuint16_t,
		z0 = svmul_m (p0, z0, svdup_u16 (4)),
		z0 = svmul_m (p0, z0, svdup_u16 (4)))

/*
** mul_4nop2_u16_m_tied1:
**	lsl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_u16_m_tied1, svuint16_t,
		z0 = svmul_n_u16_m (p0, z0, 4),
		z0 = svmul_m (p0, z0, 4))

/*
** mul_maxpownop2_u16_m_tied1:
**	lsl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_u16_m_tied1, svuint16_t,
		z0 = svmul_n_u16_m (p0, z0, MAXPOW),
		z0 = svmul_m (p0, z0, MAXPOW))

/*
** mul_1_u16_m_tied1:
**	ret
*/
TEST_UNIFORM_Z (mul_1_u16_m_tied1, svuint16_t,
		z0 = svmul_n_u16_m (p0, z0, 1),
		z0 = svmul_m (p0, z0, 1))

/*
** mul_1op1_u16_m_tied2:
**	mov	(z[0-9]+\.h), #1
**	sel	z0\.h, p0, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mul_1op1_u16_m_tied2, svuint16_t,
		z0 = svmul_u16_m (p0, svdup_u16 (1), z0),
		z0 = svmul_m (p0, svdup_u16 (1), z0))

/*
** mul_3_u16_m_tied1:
**	mov	(z[0-9]+\.h), #3
**	mul	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mul_3_u16_m_tied1, svuint16_t,
		z0 = svmul_n_u16_m (p0, z0, 3),
		z0 = svmul_m (p0, z0, 3))

/*
** mul_4dupop2_u16_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_u16_m_untied, svuint16_t,
		z0 = svmul_m (p0, z1, svdup_u16 (4)),
		z0 = svmul_m (p0, z1, svdup_u16 (4)))

/*
** mul_4nop2_u16_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_u16_m_untied, svuint16_t,
		z0 = svmul_n_u16_m (p0, z1, 4),
		z0 = svmul_m (p0, z1, 4))

/*
** mul_maxpownop2_u16_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_u16_m_untied, svuint16_t,
		z0 = svmul_n_u16_m (p0, z1, MAXPOW),
		z0 = svmul_m (p0, z1, MAXPOW))

/*
** mul_3_u16_m_untied:
**	mov	(z[0-9]+\.h), #3
**	movprfx	z0, z1
**	mul	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mul_3_u16_m_untied, svuint16_t,
		z0 = svmul_n_u16_m (p0, z1, 3),
		z0 = svmul_m (p0, z1, 3))

/*
** mul_m1_u16_m:
**	neg	z0\.h, p0/m, z0\.h
**	ret
*/
TEST_UNIFORM_Z (mul_m1_u16_m, svuint16_t,
		z0 = svmul_n_u16_m (p0, z0, -1),
		z0 = svmul_m (p0, z0, -1))

/*
** mul_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	mul	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (mul_u16_z_tied1, svuint16_t,
		z0 = svmul_u16_z (p0, z0, z1),
		z0 = svmul_z (p0, z0, z1))

/*
** mul_u16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	mul	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (mul_u16_z_tied2, svuint16_t,
		z0 = svmul_u16_z (p0, z1, z0),
		z0 = svmul_z (p0, z1, z0))

/*
** mul_u16_z_untied:
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	mul	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0\.h, p0/z, z2\.h
**	mul	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (mul_u16_z_untied, svuint16_t,
		z0 = svmul_u16_z (p0, z1, z2),
		z0 = svmul_z (p0, z1, z2))

/*
** mul_w0_u16_z_tied1:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0\.h, p0/z, z0\.h
**	mul	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (mul_w0_u16_z_tied1, svuint16_t, uint16_t,
		 z0 = svmul_n_u16_z (p0, z0, x0),
		 z0 = svmul_z (p0, z0, x0))

/*
** mul_w0_u16_z_untied:
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
TEST_UNIFORM_ZX (mul_w0_u16_z_untied, svuint16_t, uint16_t,
		 z0 = svmul_n_u16_z (p0, z1, x0),
		 z0 = svmul_z (p0, z1, x0))

/*
** mul_4dupop1_u16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1_u16_z_tied2, svuint16_t,
		z0 = svmul_z (p0, svdup_u16 (4), z0),
		z0 = svmul_z (p0, svdup_u16 (4), z0))

/*
** mul_4dupop1ptrue_u16_z_tied2:
**	lsl	z0\.h, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1ptrue_u16_z_tied2, svuint16_t,
		z0 = svmul_z (svptrue_b16 (), svdup_u16 (4), z0),
		z0 = svmul_z (svptrue_b16 (), svdup_u16 (4), z0))

/*
** mul_4dupop2_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_u16_z_tied1, svuint16_t,
		z0 = svmul_z (p0, z0, svdup_u16 (4)),
		z0 = svmul_z (p0, z0, svdup_u16 (4)))

/*
** mul_4nop2_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_u16_z_tied1, svuint16_t,
		z0 = svmul_n_u16_z (p0, z0, 4),
		z0 = svmul_z (p0, z0, 4))

/*
** mul_maxpownop2_u16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_u16_z_tied1, svuint16_t,
		z0 = svmul_n_u16_z (p0, z0, MAXPOW),
		z0 = svmul_z (p0, z0, MAXPOW))

/*
** mul_1_u16_z_tied1:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	sel	z0\.h, p0, z0\.h, z\1.h
**	ret
*/
TEST_UNIFORM_Z (mul_1_u16_z_tied1, svuint16_t,
		z0 = svmul_n_u16_z (p0, z0, 1),
		z0 = svmul_z (p0, z0, 1))

/*
** mul_1op1_u16_z_tied2:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	sel	z0\.h, p0, z0\.h, z\1.h
**	ret
*/
TEST_UNIFORM_Z (mul_1op1_u16_z_tied2, svuint16_t,
		z0 = svmul_u16_z (p0, svdup_u16 (1), z0),
		z0 = svmul_z (p0, svdup_u16 (1), z0))

/*
** mul_3_u16_z_tied1:
**	mov	(z[0-9]+\.h), #3
**	movprfx	z0\.h, p0/z, z0\.h
**	mul	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mul_3_u16_z_tied1, svuint16_t,
		z0 = svmul_n_u16_z (p0, z0, 3),
		z0 = svmul_z (p0, z0, 3))

/*
** mul_4dupop2_u16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	lsl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_u16_z_untied, svuint16_t,
		z0 = svmul_z (p0, z1, svdup_u16 (4)),
		z0 = svmul_z (p0, z1, svdup_u16 (4)))

/*
** mul_4nop2_u16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	lsl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_u16_z_untied, svuint16_t,
		z0 = svmul_n_u16_z (p0, z1, 4),
		z0 = svmul_z (p0, z1, 4))

/*
** mul_maxpownop2_u16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	lsl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_u16_z_untied, svuint16_t,
		z0 = svmul_n_u16_z (p0, z1, MAXPOW),
		z0 = svmul_z (p0, z1, MAXPOW))

/*
** mul_3_u16_z_untied:
**	mov	(z[0-9]+\.h), #3
** (
**	movprfx	z0\.h, p0/z, z1\.h
**	mul	z0\.h, p0/m, z0\.h, \1
** |
**	movprfx	z0\.h, p0/z, \1
**	mul	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (mul_3_u16_z_untied, svuint16_t,
		z0 = svmul_n_u16_z (p0, z1, 3),
		z0 = svmul_z (p0, z1, 3))

/*
** mul_u16_x_tied1:
**	mul	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (mul_u16_x_tied1, svuint16_t,
		z0 = svmul_u16_x (p0, z0, z1),
		z0 = svmul_x (p0, z0, z1))

/*
** mul_u16_x_tied2:
**	mul	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (mul_u16_x_tied2, svuint16_t,
		z0 = svmul_u16_x (p0, z1, z0),
		z0 = svmul_x (p0, z1, z0))

/*
** mul_u16_x_untied:
** (
**	movprfx	z0, z1
**	mul	z0\.h, p0/m, z0\.h, z2\.h
** |
**	movprfx	z0, z2
**	mul	z0\.h, p0/m, z0\.h, z1\.h
** )
**	ret
*/
TEST_UNIFORM_Z (mul_u16_x_untied, svuint16_t,
		z0 = svmul_u16_x (p0, z1, z2),
		z0 = svmul_x (p0, z1, z2))

/*
** mul_w0_u16_x_tied1:
**	mov	(z[0-9]+\.h), w0
**	mul	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (mul_w0_u16_x_tied1, svuint16_t, uint16_t,
		 z0 = svmul_n_u16_x (p0, z0, x0),
		 z0 = svmul_x (p0, z0, x0))

/*
** mul_w0_u16_x_untied:
**	mov	z0\.h, w0
**	mul	z0\.h, p0/m, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_ZX (mul_w0_u16_x_untied, svuint16_t, uint16_t,
		 z0 = svmul_n_u16_x (p0, z1, x0),
		 z0 = svmul_x (p0, z1, x0))

/*
** mul_4dupop1_u16_x_tied2:
**	lsl	z0\.h, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1_u16_x_tied2, svuint16_t,
		z0 = svmul_x (p0, svdup_u16 (4), z0),
		z0 = svmul_x (p0, svdup_u16 (4), z0))

/*
** mul_4dupop1ptrue_u16_x_tied2:
**	lsl	z0\.h, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1ptrue_u16_x_tied2, svuint16_t,
		z0 = svmul_x (svptrue_b16 (), svdup_u16 (4), z0),
		z0 = svmul_x (svptrue_b16 (), svdup_u16 (4), z0))

/*
** mul_4dupop2_u16_x_tied1:
**	lsl	z0\.h, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_u16_x_tied1, svuint16_t,
		z0 = svmul_x (p0, z0, svdup_u16 (4)),
		z0 = svmul_x (p0, z0, svdup_u16 (4)))

/*
** mul_4nop2_u16_x_tied1:
**	lsl	z0\.h, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_u16_x_tied1, svuint16_t,
		z0 = svmul_n_u16_x (p0, z0, 4),
		z0 = svmul_x (p0, z0, 4))

/*
** mul_maxpownop2_u16_x_tied1:
**	lsl	z0\.h, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_u16_x_tied1, svuint16_t,
		z0 = svmul_n_u16_x (p0, z0, MAXPOW),
		z0 = svmul_x (p0, z0, MAXPOW))

/*
** mul_1_u16_x_tied1:
**	ret
*/
TEST_UNIFORM_Z (mul_1_u16_x_tied1, svuint16_t,
		z0 = svmul_n_u16_x (p0, z0, 1),
		z0 = svmul_x (p0, z0, 1))

/*
** mul_1op1_u16_x_tied2:
**	ret
*/
TEST_UNIFORM_Z (mul_1op1_u16_x_tied2, svuint16_t,
		z0 = svmul_u16_x (p0, svdup_u16 (1), z0),
		z0 = svmul_x (p0, svdup_u16 (1), z0))

/*
** mul_1op1_u16_x_untied:
**	mov	z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mul_1op1_u16_x_untied, svuint16_t,
		z0 = svmul_u16_x (p0, svdup_u16 (1), z1),
		z0 = svmul_x (p0, svdup_u16 (1), z1))

/*
** mul_3_u16_x_tied1:
**	mul	z0\.h, z0\.h, #3
**	ret
*/
TEST_UNIFORM_Z (mul_3_u16_x_tied1, svuint16_t,
		z0 = svmul_n_u16_x (p0, z0, 3),
		z0 = svmul_x (p0, z0, 3))

/*
** mul_4dupop2_u16_x_untied:
**	lsl	z0\.h, z1\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_u16_x_untied, svuint16_t,
		z0 = svmul_x (p0, z1, svdup_u16 (4)),
		z0 = svmul_x (p0, z1, svdup_u16 (4)))

/*
** mul_4nop2_u16_x_untied:
**	lsl	z0\.h, z1\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_u16_x_untied, svuint16_t,
		z0 = svmul_n_u16_x (p0, z1, 4),
		z0 = svmul_x (p0, z1, 4))

/*
** mul_maxpownop2_u16_x_untied:
**	lsl	z0\.h, z1\.h, #15
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_u16_x_untied, svuint16_t,
		z0 = svmul_n_u16_x (p0, z1, MAXPOW),
		z0 = svmul_x (p0, z1, MAXPOW))

/*
** mul_3_u16_x_untied:
**	movprfx	z0, z1
**	mul	z0\.h, z0\.h, #3
**	ret
*/
TEST_UNIFORM_Z (mul_3_u16_x_untied, svuint16_t,
		z0 = svmul_n_u16_x (p0, z1, 3),
		z0 = svmul_x (p0, z1, 3))

/*
** mul_127_u16_x:
**	mul	z0\.h, z0\.h, #127
**	ret
*/
TEST_UNIFORM_Z (mul_127_u16_x, svuint16_t,
		z0 = svmul_n_u16_x (p0, z0, 127),
		z0 = svmul_x (p0, z0, 127))

/*
** mul_128_u16_x:
**	lsl	z0\.h, z0\.h, #7
**	ret
*/
TEST_UNIFORM_Z (mul_128_u16_x, svuint16_t,
		z0 = svmul_n_u16_x (p0, z0, 128),
		z0 = svmul_x (p0, z0, 128))

/*
** mul_255_u16_x:
**	mov	(z[0-9]+\.h), #255
**	mul	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mul_255_u16_x, svuint16_t,
		z0 = svmul_n_u16_x (p0, z0, 255),
		z0 = svmul_x (p0, z0, 255))

/*
** mul_m1_u16_x:
**	neg	z0\.h, p0/m, z0\.h
**	ret
*/
TEST_UNIFORM_Z (mul_m1_u16_x, svuint16_t,
		z0 = svmul_n_u16_x (p0, z0, -1),
		z0 = svmul_x (p0, z0, -1))

/*
** mul_m127_u16_x:
**	mul	z0\.h, z0\.h, #-127
**	ret
*/
TEST_UNIFORM_Z (mul_m127_u16_x, svuint16_t,
		z0 = svmul_n_u16_x (p0, z0, -127),
		z0 = svmul_x (p0, z0, -127))

/*
** mul_m128_u16_x:
**	mul	z0\.h, z0\.h, #-128
**	ret
*/
TEST_UNIFORM_Z (mul_m128_u16_x, svuint16_t,
		z0 = svmul_n_u16_x (p0, z0, -128),
		z0 = svmul_x (p0, z0, -128))
