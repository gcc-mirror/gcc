/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#define MAXPOW 1ULL<<31

/*
** mul_u32_m_tied1:
**	mul	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mul_u32_m_tied1, svuint32_t,
		z0 = svmul_u32_m (p0, z0, z1),
		z0 = svmul_m (p0, z0, z1))

/*
** mul_u32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	mul	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (mul_u32_m_tied2, svuint32_t,
		z0 = svmul_u32_m (p0, z1, z0),
		z0 = svmul_m (p0, z1, z0))

/*
** mul_u32_m_untied:
**	movprfx	z0, z1
**	mul	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (mul_u32_m_untied, svuint32_t,
		z0 = svmul_u32_m (p0, z1, z2),
		z0 = svmul_m (p0, z1, z2))

/*
** mul_w0_u32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	mul	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (mul_w0_u32_m_tied1, svuint32_t, uint32_t,
		 z0 = svmul_n_u32_m (p0, z0, x0),
		 z0 = svmul_m (p0, z0, x0))

/*
** mul_w0_u32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	mul	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (mul_w0_u32_m_untied, svuint32_t, uint32_t,
		 z0 = svmul_n_u32_m (p0, z1, x0),
		 z0 = svmul_m (p0, z1, x0))

/*
** mul_4dupop1_u32_m_tied2:
**	mov	(z[0-9]+)\.s, #4
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, \1
**	mul	z0\.s, p0/m, z0\.s, \2\.s
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1_u32_m_tied2, svuint32_t,
		z0 = svmul_m (p0, svdup_u32 (4), z0),
		z0 = svmul_m (p0, svdup_u32 (4), z0))

/*
** mul_4dupop1ptrue_u32_m_tied2:
**	lsl	z0\.s, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1ptrue_u32_m_tied2, svuint32_t,
		z0 = svmul_m (svptrue_b32 (), svdup_u32 (4), z0),
		z0 = svmul_m (svptrue_b32 (), svdup_u32 (4), z0))

/*
** mul_4dupop2_u32_m_tied1:
**	lsl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_u32_m_tied1, svuint32_t,
		z0 = svmul_m (p0, z0, svdup_u32 (4)),
		z0 = svmul_m (p0, z0, svdup_u32 (4)))

/*
** mul_4nop2_u32_m_tied1:
**	lsl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_u32_m_tied1, svuint32_t,
		z0 = svmul_n_u32_m (p0, z0, 4),
		z0 = svmul_m (p0, z0, 4))

/*
** mul_maxpownop2_u32_m_tied1:
**	lsl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_u32_m_tied1, svuint32_t,
		z0 = svmul_n_u32_m (p0, z0, MAXPOW),
		z0 = svmul_m (p0, z0, MAXPOW))

/*
** mul_1_u32_m_tied1:
**	ret
*/
TEST_UNIFORM_Z (mul_1_u32_m_tied1, svuint32_t,
		z0 = svmul_n_u32_m (p0, z0, 1),
		z0 = svmul_m (p0, z0, 1))

/*
** mul_1op1_u32_m_tied2:
**	mov	(z[0-9]+\.s), #1
**	sel	z0\.s, p0, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (mul_1op1_u32_m_tied2, svuint32_t,
		z0 = svmul_u32_m (p0, svdup_u32 (1), z0),
		z0 = svmul_m (p0, svdup_u32 (1), z0))

/*
** mul_3_u32_m_tied1:
**	mov	(z[0-9]+\.s), #3
**	mul	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (mul_3_u32_m_tied1, svuint32_t,
		z0 = svmul_n_u32_m (p0, z0, 3),
		z0 = svmul_m (p0, z0, 3))

/*
** mul_4dupop2_u32_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_u32_m_untied, svuint32_t,
		z0 = svmul_m (p0, z1, svdup_u32 (4)),
		z0 = svmul_m (p0, z1, svdup_u32 (4)))

/*
** mul_4nop2_u32_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_u32_m_untied, svuint32_t,
		z0 = svmul_n_u32_m (p0, z1, 4),
		z0 = svmul_m (p0, z1, 4))

/*
** mul_maxpownop2_u32_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_u32_m_untied, svuint32_t,
		z0 = svmul_n_u32_m (p0, z1, MAXPOW),
		z0 = svmul_m (p0, z1, MAXPOW))

/*
** mul_3_u32_m_untied:
**	mov	(z[0-9]+\.s), #3
**	movprfx	z0, z1
**	mul	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (mul_3_u32_m_untied, svuint32_t,
		z0 = svmul_n_u32_m (p0, z1, 3),
		z0 = svmul_m (p0, z1, 3))

/*
** mul_m1_u32_m:
**	mov	(z[0-9]+)\.b, #-1
**	mul	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (mul_m1_u32_m, svuint32_t,
		z0 = svmul_n_u32_m (p0, z0, -1),
		z0 = svmul_m (p0, z0, -1))

/*
** mul_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	mul	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mul_u32_z_tied1, svuint32_t,
		z0 = svmul_u32_z (p0, z0, z1),
		z0 = svmul_z (p0, z0, z1))

/*
** mul_u32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	mul	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mul_u32_z_tied2, svuint32_t,
		z0 = svmul_u32_z (p0, z1, z0),
		z0 = svmul_z (p0, z1, z0))

/*
** mul_u32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	mul	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	mul	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (mul_u32_z_untied, svuint32_t,
		z0 = svmul_u32_z (p0, z1, z2),
		z0 = svmul_z (p0, z1, z2))

/*
** mul_w0_u32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	mul	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (mul_w0_u32_z_tied1, svuint32_t, uint32_t,
		 z0 = svmul_n_u32_z (p0, z0, x0),
		 z0 = svmul_z (p0, z0, x0))

/*
** mul_w0_u32_z_untied:
**	mov	(z[0-9]+\.s), w0
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	mul	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	mul	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_ZX (mul_w0_u32_z_untied, svuint32_t, uint32_t,
		 z0 = svmul_n_u32_z (p0, z1, x0),
		 z0 = svmul_z (p0, z1, x0))

/*
** mul_4dupop1_u32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1_u32_z_tied2, svuint32_t,
		z0 = svmul_z (p0, svdup_u32 (4), z0),
		z0 = svmul_z (p0, svdup_u32 (4), z0))

/*
** mul_4dupop1ptrue_u32_z_tied2:
**	lsl	z0\.s, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1ptrue_u32_z_tied2, svuint32_t,
		z0 = svmul_z (svptrue_b32 (), svdup_u32 (4), z0),
		z0 = svmul_z (svptrue_b32 (), svdup_u32 (4), z0))

/*
** mul_4dupop2_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_u32_z_tied1, svuint32_t,
		z0 = svmul_z (p0, z0, svdup_u32 (4)),
		z0 = svmul_z (p0, z0, svdup_u32 (4)))

/*
** mul_4nop2_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_u32_z_tied1, svuint32_t,
		z0 = svmul_n_u32_z (p0, z0, 4),
		z0 = svmul_z (p0, z0, 4))

/*
** mul_maxpownop2_u32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_u32_z_tied1, svuint32_t,
		z0 = svmul_n_u32_z (p0, z0, MAXPOW),
		z0 = svmul_z (p0, z0, MAXPOW))

/*
** mul_1_u32_z_tied1:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	sel	z0\.s, p0, z0\.s, z\1.s
**	ret
*/
TEST_UNIFORM_Z (mul_1_u32_z_tied1, svuint32_t,
		z0 = svmul_n_u32_z (p0, z0, 1),
		z0 = svmul_z (p0, z0, 1))

/*
** mul_1op1_u32_z_tied2:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	sel	z0\.s, p0, z0\.s, z\1.s
**	ret
*/
TEST_UNIFORM_Z (mul_1op1_u32_z_tied2, svuint32_t,
		z0 = svmul_u32_z (p0, svdup_u32 (1), z0),
		z0 = svmul_z (p0, svdup_u32 (1), z0))

/*
** mul_3_u32_z_tied1:
**	mov	(z[0-9]+\.s), #3
**	movprfx	z0\.s, p0/z, z0\.s
**	mul	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (mul_3_u32_z_tied1, svuint32_t,
		z0 = svmul_n_u32_z (p0, z0, 3),
		z0 = svmul_z (p0, z0, 3))

/*
** mul_4dupop2_u32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	lsl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_u32_z_untied, svuint32_t,
		z0 = svmul_z (p0, z1, svdup_u32 (4)),
		z0 = svmul_z (p0, z1, svdup_u32 (4)))

/*
** mul_4nop2_u32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	lsl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_u32_z_untied, svuint32_t,
		z0 = svmul_n_u32_z (p0, z1, 4),
		z0 = svmul_z (p0, z1, 4))

/*
** mul_maxpownop2_u32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	lsl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_u32_z_untied, svuint32_t,
		z0 = svmul_n_u32_z (p0, z1, MAXPOW),
		z0 = svmul_z (p0, z1, MAXPOW))

/*
** mul_3_u32_z_untied:
**	mov	(z[0-9]+\.s), #3
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	mul	z0\.s, p0/m, z0\.s, \1
** |
**	movprfx	z0\.s, p0/z, \1
**	mul	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (mul_3_u32_z_untied, svuint32_t,
		z0 = svmul_n_u32_z (p0, z1, 3),
		z0 = svmul_z (p0, z1, 3))

/*
** mul_u32_x_tied1:
**	mul	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mul_u32_x_tied1, svuint32_t,
		z0 = svmul_u32_x (p0, z0, z1),
		z0 = svmul_x (p0, z0, z1))

/*
** mul_u32_x_tied2:
**	mul	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mul_u32_x_tied2, svuint32_t,
		z0 = svmul_u32_x (p0, z1, z0),
		z0 = svmul_x (p0, z1, z0))

/*
** mul_u32_x_untied:
** (
**	movprfx	z0, z1
**	mul	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0, z2
**	mul	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (mul_u32_x_untied, svuint32_t,
		z0 = svmul_u32_x (p0, z1, z2),
		z0 = svmul_x (p0, z1, z2))

/*
** mul_w0_u32_x_tied1:
**	mov	(z[0-9]+\.s), w0
**	mul	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (mul_w0_u32_x_tied1, svuint32_t, uint32_t,
		 z0 = svmul_n_u32_x (p0, z0, x0),
		 z0 = svmul_x (p0, z0, x0))

/*
** mul_w0_u32_x_untied:
**	mov	z0\.s, w0
**	mul	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZX (mul_w0_u32_x_untied, svuint32_t, uint32_t,
		 z0 = svmul_n_u32_x (p0, z1, x0),
		 z0 = svmul_x (p0, z1, x0))

/*
** mul_4dupop1_u32_x_tied2:
**	lsl	z0\.s, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1_u32_x_tied2, svuint32_t,
		z0 = svmul_x (p0, svdup_u32 (4), z0),
		z0 = svmul_x (p0, svdup_u32 (4), z0))

/*
** mul_4dupop1ptrue_u32_x_tied2:
**	lsl	z0\.s, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1ptrue_u32_x_tied2, svuint32_t,
		z0 = svmul_x (svptrue_b32 (), svdup_u32 (4), z0),
		z0 = svmul_x (svptrue_b32 (), svdup_u32 (4), z0))

/*
** mul_4dupop2_u32_x_tied1:
**	lsl	z0\.s, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_u32_x_tied1, svuint32_t,
		z0 = svmul_x (p0, z0, svdup_u32 (4)),
		z0 = svmul_x (p0, z0, svdup_u32 (4)))

/*
** mul_4nop2_u32_x_tied1:
**	lsl	z0\.s, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_u32_x_tied1, svuint32_t,
		z0 = svmul_n_u32_x (p0, z0, 4),
		z0 = svmul_x (p0, z0, 4))

/*
** mul_maxpownop2_u32_x_tied1:
**	lsl	z0\.s, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_u32_x_tied1, svuint32_t,
		z0 = svmul_n_u32_x (p0, z0, MAXPOW),
		z0 = svmul_x (p0, z0, MAXPOW))

/*
** mul_1_u32_x_tied1:
**	ret
*/
TEST_UNIFORM_Z (mul_1_u32_x_tied1, svuint32_t,
		z0 = svmul_n_u32_x (p0, z0, 1),
		z0 = svmul_x (p0, z0, 1))

/*
** mul_1op1_u32_x_tied2:
**	ret
*/
TEST_UNIFORM_Z (mul_1op1_u32_x_tied2, svuint32_t,
		z0 = svmul_u32_x (p0, svdup_u32 (1), z0),
		z0 = svmul_x (p0, svdup_u32 (1), z0))

/*
** mul_1op1_u32_x_untied:
**	mov	z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mul_1op1_u32_x_untied, svuint32_t,
		z0 = svmul_u32_x (p0, svdup_u32 (1), z1),
		z0 = svmul_x (p0, svdup_u32 (1), z1))

/*
** mul_3_u32_x_tied1:
**	mul	z0\.s, z0\.s, #3
**	ret
*/
TEST_UNIFORM_Z (mul_3_u32_x_tied1, svuint32_t,
		z0 = svmul_n_u32_x (p0, z0, 3),
		z0 = svmul_x (p0, z0, 3))

/*
** mul_4dupop2_u32_x_untied:
**	lsl	z0\.s, z1\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_u32_x_untied, svuint32_t,
		z0 = svmul_x (p0, z1, svdup_u32 (4)),
		z0 = svmul_x (p0, z1, svdup_u32 (4)))

/*
** mul_4nop2_u32_x_untied:
**	lsl	z0\.s, z1\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_u32_x_untied, svuint32_t,
		z0 = svmul_n_u32_x (p0, z1, 4),
		z0 = svmul_x (p0, z1, 4))

/*
** mul_maxpownop2_u32_x_untied:
**	lsl	z0\.s, z1\.s, #31
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_u32_x_untied, svuint32_t,
		z0 = svmul_n_u32_x (p0, z1, MAXPOW),
		z0 = svmul_x (p0, z1, MAXPOW))

/*
** mul_3_u32_x_untied:
**	movprfx	z0, z1
**	mul	z0\.s, z0\.s, #3
**	ret
*/
TEST_UNIFORM_Z (mul_3_u32_x_untied, svuint32_t,
		z0 = svmul_n_u32_x (p0, z1, 3),
		z0 = svmul_x (p0, z1, 3))

/*
** mul_127_u32_x:
**	mul	z0\.s, z0\.s, #127
**	ret
*/
TEST_UNIFORM_Z (mul_127_u32_x, svuint32_t,
		z0 = svmul_n_u32_x (p0, z0, 127),
		z0 = svmul_x (p0, z0, 127))

/*
** mul_128_u32_x:
**	lsl	z0\.s, z0\.s, #7
**	ret
*/
TEST_UNIFORM_Z (mul_128_u32_x, svuint32_t,
		z0 = svmul_n_u32_x (p0, z0, 128),
		z0 = svmul_x (p0, z0, 128))

/*
** mul_255_u32_x:
**	mov	(z[0-9]+\.s), #255
**	mul	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (mul_255_u32_x, svuint32_t,
		z0 = svmul_n_u32_x (p0, z0, 255),
		z0 = svmul_x (p0, z0, 255))

/*
** mul_m1_u32_x:
**	mul	z0\.s, z0\.s, #-1
**	ret
*/
TEST_UNIFORM_Z (mul_m1_u32_x, svuint32_t,
		z0 = svmul_n_u32_x (p0, z0, -1),
		z0 = svmul_x (p0, z0, -1))

/*
** mul_m127_u32_x:
**	mul	z0\.s, z0\.s, #-127
**	ret
*/
TEST_UNIFORM_Z (mul_m127_u32_x, svuint32_t,
		z0 = svmul_n_u32_x (p0, z0, -127),
		z0 = svmul_x (p0, z0, -127))

/*
** mul_m128_u32_x:
**	mul	z0\.s, z0\.s, #-128
**	ret
*/
TEST_UNIFORM_Z (mul_m128_u32_x, svuint32_t,
		z0 = svmul_n_u32_x (p0, z0, -128),
		z0 = svmul_x (p0, z0, -128))
