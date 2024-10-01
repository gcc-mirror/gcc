/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#define MAXPOW 1ULL<<14

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
** mul_w0_s16_m_untied:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	mul	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_ZX (mul_w0_s16_m_untied, svint16_t, int16_t,
		 z0 = svmul_n_s16_m (p0, z1, x0),
		 z0 = svmul_m (p0, z1, x0))

/*
** mul_4dupop1_s16_m_tied2:
**	mov	(z[0-9]+)\.h, #4
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, \1
**	mul	z0\.h, p0/m, z0\.h, \2\.h
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1_s16_m_tied2, svint16_t,
		z0 = svmul_m (p0, svdup_s16 (4), z0),
		z0 = svmul_m (p0, svdup_s16 (4), z0))

/*
** mul_4dupop1ptrue_s16_m_tied2:
**	lsl	z0\.h, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1ptrue_s16_m_tied2, svint16_t,
		z0 = svmul_m (svptrue_b16 (), svdup_s16 (4), z0),
		z0 = svmul_m (svptrue_b16 (), svdup_s16 (4), z0))

/*
** mul_4dupop2_s16_m_tied1:
**	lsl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_s16_m_tied1, svint16_t,
		z0 = svmul_m (p0, z0, svdup_s16 (4)),
		z0 = svmul_m (p0, z0, svdup_s16 (4)))

/*
** mul_4nop2_s16_m_tied1:
**	lsl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_s16_m_tied1, svint16_t,
		z0 = svmul_n_s16_m (p0, z0, 4),
		z0 = svmul_m (p0, z0, 4))

/*
** mul_maxpownop2_s16_m_tied1:
**	lsl	z0\.h, p0/m, z0\.h, #14
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_s16_m_tied1, svint16_t,
		z0 = svmul_n_s16_m (p0, z0, MAXPOW),
		z0 = svmul_m (p0, z0, MAXPOW))

/*
** mul_intminnop2_s16_m_tied1:
**	lsl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (mul_intminnop2_s16_m_tied1, svint16_t,
		z0 = svmul_n_s16_m (p0, z0, INT16_MIN),
		z0 = svmul_m (p0, z0, INT16_MIN))

/*
** mul_1_s16_m_tied1:
**	ret
*/
TEST_UNIFORM_Z (mul_1_s16_m_tied1, svint16_t,
		z0 = svmul_n_s16_m (p0, z0, 1),
		z0 = svmul_m (p0, z0, 1))

/*
** mul_1op1_s16_m_tied2:
**	mov	(z[0-9]+\.h), #1
**	sel	z0\.h, p0, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mul_1op1_s16_m_tied2, svint16_t,
		z0 = svmul_s16_m (p0, svdup_s16 (1), z0),
		z0 = svmul_m (p0, svdup_s16 (1), z0))

/*
** mul_3_s16_m_tied1:
**	mov	(z[0-9]+\.h), #3
**	mul	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mul_3_s16_m_tied1, svint16_t,
		z0 = svmul_n_s16_m (p0, z0, 3),
		z0 = svmul_m (p0, z0, 3))

/*
** mul_4dupop2_s16_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_s16_m_untied, svint16_t,
		z0 = svmul_m (p0, z1, svdup_s16 (4)),
		z0 = svmul_m (p0, z1, svdup_s16 (4)))

/*
** mul_4nop2_s16_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_s16_m_untied, svint16_t,
		z0 = svmul_n_s16_m (p0, z1, 4),
		z0 = svmul_m (p0, z1, 4))

/*
** mul_maxpownop2_s16_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.h, p0/m, z0\.h, #14
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_s16_m_untied, svint16_t,
		z0 = svmul_n_s16_m (p0, z1, MAXPOW),
		z0 = svmul_m (p0, z1, MAXPOW))

/*
** mul_3_s16_m_untied:
**	mov	(z[0-9]+\.h), #3
**	movprfx	z0, z1
**	mul	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mul_3_s16_m_untied, svint16_t,
		z0 = svmul_n_s16_m (p0, z1, 3),
		z0 = svmul_m (p0, z1, 3))

/*
** mul_m1_s16_m:
**	neg	z0\.h, p0/m, z0\.h
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
** mul_4dupop1_s16_z_tied2:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1_s16_z_tied2, svint16_t,
		z0 = svmul_z (p0, svdup_s16 (4), z0),
		z0 = svmul_z (p0, svdup_s16 (4), z0))

/*
** mul_4dupop1ptrue_s16_z_tied2:
**	lsl	z0\.h, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1ptrue_s16_z_tied2, svint16_t,
		z0 = svmul_z (svptrue_b16 (), svdup_s16 (4), z0),
		z0 = svmul_z (svptrue_b16 (), svdup_s16 (4), z0))

/*
** mul_4dupop2_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_s16_z_tied1, svint16_t,
		z0 = svmul_z (p0, z0, svdup_s16 (4)),
		z0 = svmul_z (p0, z0, svdup_s16 (4)))

/*
** mul_4nop2_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_s16_z_tied1, svint16_t,
		z0 = svmul_n_s16_z (p0, z0, 4),
		z0 = svmul_z (p0, z0, 4))

/*
** mul_maxpownop2_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsl	z0\.h, p0/m, z0\.h, #14
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_s16_z_tied1, svint16_t,
		z0 = svmul_n_s16_z (p0, z0, MAXPOW),
		z0 = svmul_z (p0, z0, MAXPOW))

/*
** mul_intminnop2_s16_z_tied1:
**	movprfx	z0\.h, p0/z, z0\.h
**	lsl	z0\.h, p0/m, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (mul_intminnop2_s16_z_tied1, svint16_t,
		z0 = svmul_n_s16_z (p0, z0, INT16_MIN),
		z0 = svmul_z (p0, z0, INT16_MIN))

/*
** mul_1_s16_z_tied1:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	sel	z0\.h, p0, z0\.h, z\1.h
**	ret
*/
TEST_UNIFORM_Z (mul_1_s16_z_tied1, svint16_t,
		z0 = svmul_n_s16_z (p0, z0, 1),
		z0 = svmul_z (p0, z0, 1))

/*
** mul_1op1_s16_z_tied2:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	sel	z0\.h, p0, z0\.h, z\1.h
**	ret
*/
TEST_UNIFORM_Z (mul_1op1_s16_z_tied2, svint16_t,
		z0 = svmul_s16_z (p0, svdup_s16 (1), z0),
		z0 = svmul_z (p0, svdup_s16 (1), z0))

/*
** mul_3_s16_z_tied1:
**	mov	(z[0-9]+\.h), #3
**	movprfx	z0\.h, p0/z, z0\.h
**	mul	z0\.h, p0/m, z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (mul_3_s16_z_tied1, svint16_t,
		z0 = svmul_n_s16_z (p0, z0, 3),
		z0 = svmul_z (p0, z0, 3))

/*
** mul_4dupop2_s16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	lsl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_s16_z_untied, svint16_t,
		z0 = svmul_z (p0, z1, svdup_s16 (4)),
		z0 = svmul_z (p0, z1, svdup_s16 (4)))

/*
** mul_4nop2_s16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	lsl	z0\.h, p0/m, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_s16_z_untied, svint16_t,
		z0 = svmul_n_s16_z (p0, z1, 4),
		z0 = svmul_z (p0, z1, 4))

/*
** mul_maxpownop2_s16_z_untied:
**	movprfx	z0\.h, p0/z, z1\.h
**	lsl	z0\.h, p0/m, z0\.h, #14
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_s16_z_untied, svint16_t,
		z0 = svmul_n_s16_z (p0, z1, MAXPOW),
		z0 = svmul_z (p0, z1, MAXPOW))

/*
** mul_3_s16_z_untied:
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
TEST_UNIFORM_Z (mul_3_s16_z_untied, svint16_t,
		z0 = svmul_n_s16_z (p0, z1, 3),
		z0 = svmul_z (p0, z1, 3))

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
** mul_4dupop1_s16_x_tied2:
**	lsl	z0\.h, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1_s16_x_tied2, svint16_t,
		z0 = svmul_x (p0, svdup_s16 (4), z0),
		z0 = svmul_x (p0, svdup_s16 (4), z0))

/*
** mul_4dupop1ptrue_s16_x_tied2:
**	lsl	z0\.h, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1ptrue_s16_x_tied2, svint16_t,
		z0 = svmul_x (svptrue_b16 (), svdup_s16 (4), z0),
		z0 = svmul_x (svptrue_b16 (), svdup_s16 (4), z0))

/*
** mul_4dupop2_s16_x_tied1:
**	lsl	z0\.h, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_s16_x_tied1, svint16_t,
		z0 = svmul_x (p0, z0, svdup_s16 (4)),
		z0 = svmul_x (p0, z0, svdup_s16 (4)))

/*
** mul_4nop2_s16_x_tied1:
**	lsl	z0\.h, z0\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_s16_x_tied1, svint16_t,
		z0 = svmul_n_s16_x (p0, z0, 4),
		z0 = svmul_x (p0, z0, 4))

/*
** mul_maxpownop2_s16_x_tied1:
**	lsl	z0\.h, z0\.h, #14
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_s16_x_tied1, svint16_t,
		z0 = svmul_n_s16_x (p0, z0, MAXPOW),
		z0 = svmul_x (p0, z0, MAXPOW))

/*
** mul_intminnop2_s16_x_tied1:
**	lsl	z0\.h, z0\.h, #15
**	ret
*/
TEST_UNIFORM_Z (mul_intminnop2_s16_x_tied1, svint16_t,
		z0 = svmul_n_s16_x (p0, z0, INT16_MIN),
		z0 = svmul_x (p0, z0, INT16_MIN))

/*
** mul_1_s16_x_tied1:
**	ret
*/
TEST_UNIFORM_Z (mul_1_s16_x_tied1, svint16_t,
		z0 = svmul_n_s16_x (p0, z0, 1),
		z0 = svmul_x (p0, z0, 1))

/*
** mul_1op1_s16_x_tied2:
**	ret
*/
TEST_UNIFORM_Z (mul_1op1_s16_x_tied2, svint16_t,
		z0 = svmul_s16_x (p0, svdup_s16 (1), z0),
		z0 = svmul_x (p0, svdup_s16 (1), z0))

/*
** mul_1op1_s16_x_untied:
**	mov	z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mul_1op1_s16_x_untied, svint16_t,
		z0 = svmul_s16_x (p0, svdup_s16 (1), z1),
		z0 = svmul_x (p0, svdup_s16 (1), z1))

/*
** mul_3_s16_x_tied1:
**	mul	z0\.h, z0\.h, #3
**	ret
*/
TEST_UNIFORM_Z (mul_3_s16_x_tied1, svint16_t,
		z0 = svmul_n_s16_x (p0, z0, 3),
		z0 = svmul_x (p0, z0, 3))

/*
** mul_4dupop2_s16_x_untied:
**	lsl	z0\.h, z1\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_s16_x_untied, svint16_t,
		z0 = svmul_x (p0, z1, svdup_s16 (4)),
		z0 = svmul_x (p0, z1, svdup_s16 (4)))

/*
** mul_4nop2_s16_x_untied:
**	lsl	z0\.h, z1\.h, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_s16_x_untied, svint16_t,
		z0 = svmul_n_s16_x (p0, z1, 4),
		z0 = svmul_x (p0, z1, 4))

/*
** mul_maxpownop2_s16_x_untied:
**	lsl	z0\.h, z1\.h, #14
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_s16_x_untied, svint16_t,
		z0 = svmul_n_s16_x (p0, z1, MAXPOW),
		z0 = svmul_x (p0, z1, MAXPOW))

/*
** mul_3_s16_x_untied:
**	movprfx	z0, z1
**	mul	z0\.h, z0\.h, #3
**	ret
*/
TEST_UNIFORM_Z (mul_3_s16_x_untied, svint16_t,
		z0 = svmul_n_s16_x (p0, z1, 3),
		z0 = svmul_x (p0, z1, 3))

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
**	lsl	z0\.h, z0\.h, #7
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
**	neg	z0\.h, p0/m, z0\.h
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
