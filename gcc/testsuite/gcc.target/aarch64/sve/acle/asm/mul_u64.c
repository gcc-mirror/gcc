/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#define MAXPOW 1ULL<<63

/*
** mul_u64_m_tied1:
**	mul	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mul_u64_m_tied1, svuint64_t,
		z0 = svmul_u64_m (p0, z0, z1),
		z0 = svmul_m (p0, z0, z1))

/*
** mul_u64_m_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	mul	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (mul_u64_m_tied2, svuint64_t,
		z0 = svmul_u64_m (p0, z1, z0),
		z0 = svmul_m (p0, z1, z0))

/*
** mul_u64_m_untied:
**	movprfx	z0, z1
**	mul	z0\.d, p0/m, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (mul_u64_m_untied, svuint64_t,
		z0 = svmul_u64_m (p0, z1, z2),
		z0 = svmul_m (p0, z1, z2))

/*
** mul_x0_u64_m_tied1:
**	mov	(z[0-9]+\.d), x0
**	mul	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (mul_x0_u64_m_tied1, svuint64_t, uint64_t,
		 z0 = svmul_n_u64_m (p0, z0, x0),
		 z0 = svmul_m (p0, z0, x0))

/*
** mul_x0_u64_m_untied:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0, z1
**	mul	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (mul_x0_u64_m_untied, svuint64_t, uint64_t,
		 z0 = svmul_n_u64_m (p0, z1, x0),
		 z0 = svmul_m (p0, z1, x0))

/*
** mul_4dupop1_u64_m_tied2:
**	mov	(z[0-9]+)\.d, #4
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, \1
**	mul	z0\.d, p0/m, z0\.d, \2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1_u64_m_tied2, svuint64_t,
		z0 = svmul_m (p0, svdup_u64 (4), z0),
		z0 = svmul_m (p0, svdup_u64 (4), z0))

/*
** mul_4dupop1ptrue_u64_m_tied2:
**	lsl	z0\.d, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1ptrue_u64_m_tied2, svuint64_t,
		z0 = svmul_m (svptrue_b64 (), svdup_u64 (4), z0),
		z0 = svmul_m (svptrue_b64 (), svdup_u64 (4), z0))

/*
** mul_4dupop2_u64_m_tied1:
**	lsl	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_u64_m_tied1, svuint64_t,
		z0 = svmul_m (p0, z0, svdup_u64 (4)),
		z0 = svmul_m (p0, z0, svdup_u64 (4)))

/*
** mul_4nop2_u64_m_tied1:
**	lsl	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_u64_m_tied1, svuint64_t,
		z0 = svmul_n_u64_m (p0, z0, 4),
		z0 = svmul_m (p0, z0, 4))

/*
** mul_maxpownop2_u64_m_tied1:
**	lsl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_u64_m_tied1, svuint64_t,
		z0 = svmul_n_u64_m (p0, z0, MAXPOW),
		z0 = svmul_m (p0, z0, MAXPOW))

/*
** mul_1_u64_m_tied1:
**	ret
*/
TEST_UNIFORM_Z (mul_1_u64_m_tied1, svuint64_t,
		z0 = svmul_n_u64_m (p0, z0, 1),
		z0 = svmul_m (p0, z0, 1))

/*
** mul_1op1_u64_m_tied2:
**	mov	(z[0-9]+\.d), #1
**	sel	z0\.d, p0, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (mul_1op1_u64_m_tied2, svuint64_t,
		z0 = svmul_u64_m (p0, svdup_u64 (1), z0),
		z0 = svmul_m (p0, svdup_u64 (1), z0))

/*
** mul_2_u64_m_tied1:
**	lsl	z0\.d, p0/m, z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (mul_2_u64_m_tied1, svuint64_t,
		z0 = svmul_n_u64_m (p0, z0, 2),
		z0 = svmul_m (p0, z0, 2))

/*
** mul_3_u64_m_tied1:
**	mov	(z[0-9]+\.d), #3
**	mul	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (mul_3_u64_m_tied1, svuint64_t,
		z0 = svmul_n_u64_m (p0, z0, 3),
		z0 = svmul_m (p0, z0, 3))

/*
** mul_4dupop2_u64_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_u64_m_untied, svuint64_t,
		z0 = svmul_m (p0, z1, svdup_u64 (4)),
		z0 = svmul_m (p0, z1, svdup_u64 (4)))

/*
** mul_4nop2_u64_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_u64_m_untied, svuint64_t,
		z0 = svmul_n_u64_m (p0, z1, 4),
		z0 = svmul_m (p0, z1, 4))

/*
** mul_maxpownop2_u64_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_u64_m_untied, svuint64_t,
		z0 = svmul_n_u64_m (p0, z1, MAXPOW),
		z0 = svmul_m (p0, z1, MAXPOW))

/*
** mul_3_u64_m_untied:
**	mov	(z[0-9]+\.d), #3
**	movprfx	z0, z1
**	mul	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (mul_3_u64_m_untied, svuint64_t,
		z0 = svmul_n_u64_m (p0, z1, 3),
		z0 = svmul_m (p0, z1, 3))

/*
** mul_m1_u64_m:
**	mov	(z[0-9]+)\.b, #-1
**	mul	z0\.d, p0/m, z0\.d, \1\.d
**	ret
*/
TEST_UNIFORM_Z (mul_m1_u64_m, svuint64_t,
		z0 = svmul_n_u64_m (p0, z0, -1),
		z0 = svmul_m (p0, z0, -1))

/*
** mul_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	mul	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mul_u64_z_tied1, svuint64_t,
		z0 = svmul_u64_z (p0, z0, z1),
		z0 = svmul_z (p0, z0, z1))

/*
** mul_u64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	mul	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mul_u64_z_tied2, svuint64_t,
		z0 = svmul_u64_z (p0, z1, z0),
		z0 = svmul_z (p0, z1, z0))

/*
** mul_u64_z_untied:
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	mul	z0\.d, p0/m, z0\.d, z2\.d
** |
**	movprfx	z0\.d, p0/z, z2\.d
**	mul	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (mul_u64_z_untied, svuint64_t,
		z0 = svmul_u64_z (p0, z1, z2),
		z0 = svmul_z (p0, z1, z2))

/*
** mul_x0_u64_z_tied1:
**	mov	(z[0-9]+\.d), x0
**	movprfx	z0\.d, p0/z, z0\.d
**	mul	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (mul_x0_u64_z_tied1, svuint64_t, uint64_t,
		 z0 = svmul_n_u64_z (p0, z0, x0),
		 z0 = svmul_z (p0, z0, x0))

/*
** mul_x0_u64_z_untied:
**	mov	(z[0-9]+\.d), x0
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	mul	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	mul	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_ZX (mul_x0_u64_z_untied, svuint64_t, uint64_t,
		 z0 = svmul_n_u64_z (p0, z1, x0),
		 z0 = svmul_z (p0, z1, x0))

/*
** mul_4dupop1_u64_z_tied2:
**	movprfx	z0\.d, p0/z, z0\.d
**	lsl	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1_u64_z_tied2, svuint64_t,
		z0 = svmul_z (p0, svdup_u64 (4), z0),
		z0 = svmul_z (p0, svdup_u64 (4), z0))

/*
** mul_4dupop1ptrue_u64_z_tied2:
**	lsl	z0\.d, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1ptrue_u64_z_tied2, svuint64_t,
		z0 = svmul_z (svptrue_b64 (), svdup_u64 (4), z0),
		z0 = svmul_z (svptrue_b64 (), svdup_u64 (4), z0))

/*
** mul_4dupop2_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	lsl	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_u64_z_tied1, svuint64_t,
		z0 = svmul_z (p0, z0, svdup_u64 (4)),
		z0 = svmul_z (p0, z0, svdup_u64 (4)))

/*
** mul_4nop2_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	lsl	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_u64_z_tied1, svuint64_t,
		z0 = svmul_n_u64_z (p0, z0, 4),
		z0 = svmul_z (p0, z0, 4))

/*
** mul_maxpownop2_u64_z_tied1:
**	movprfx	z0\.d, p0/z, z0\.d
**	lsl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_u64_z_tied1, svuint64_t,
		z0 = svmul_n_u64_z (p0, z0, MAXPOW),
		z0 = svmul_z (p0, z0, MAXPOW))

/*
** mul_1_u64_z_tied1:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	sel	z0\.d, p0, z0\.d, z\1.d
**	ret
*/
TEST_UNIFORM_Z (mul_1_u64_z_tied1, svuint64_t,
		z0 = svmul_n_u64_z (p0, z0, 1),
		z0 = svmul_z (p0, z0, 1))

/*
** mul_1op1_u64_z_tied2:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	sel	z0\.d, p0, z0\.d, z\1.d
**	ret
*/
TEST_UNIFORM_Z (mul_1op1_u64_z_tied2, svuint64_t,
		z0 = svmul_u64_z (p0, svdup_u64 (1), z0),
		z0 = svmul_z (p0, svdup_u64 (1), z0))

/*
** mul_2_u64_z_tied1:
**	movprfx	z0.d, p0/z, z0.d
**	lsl	z0.d, p0/m, z0.d, #1
**	ret
*/
TEST_UNIFORM_Z (mul_2_u64_z_tied1, svuint64_t,
		z0 = svmul_n_u64_z (p0, z0, 2),
		z0 = svmul_z (p0, z0, 2))

/*
** mul_3_u64_z_tied1:
**	mov	(z[0-9]+\.d), #3
**	movprfx	z0\.d, p0/z, z0\.d
**	mul	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (mul_3_u64_z_tied1, svuint64_t,
		z0 = svmul_n_u64_z (p0, z0, 3),
		z0 = svmul_z (p0, z0, 3))

/*
** mul_4dupop2_u64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	lsl	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_u64_z_untied, svuint64_t,
		z0 = svmul_z (p0, z1, svdup_u64 (4)),
		z0 = svmul_z (p0, z1, svdup_u64 (4)))

/*
** mul_4nop2_u64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	lsl	z0\.d, p0/m, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_u64_z_untied, svuint64_t,
		z0 = svmul_n_u64_z (p0, z1, 4),
		z0 = svmul_z (p0, z1, 4))

/*
** mul_maxpownop2_u64_z_untied:
**	movprfx	z0\.d, p0/z, z1\.d
**	lsl	z0\.d, p0/m, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_u64_z_untied, svuint64_t,
		z0 = svmul_n_u64_z (p0, z1, MAXPOW),
		z0 = svmul_z (p0, z1, MAXPOW))

/*
** mul_3_u64_z_untied:
**	mov	(z[0-9]+\.d), #3
** (
**	movprfx	z0\.d, p0/z, z1\.d
**	mul	z0\.d, p0/m, z0\.d, \1
** |
**	movprfx	z0\.d, p0/z, \1
**	mul	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (mul_3_u64_z_untied, svuint64_t,
		z0 = svmul_n_u64_z (p0, z1, 3),
		z0 = svmul_z (p0, z1, 3))

/*
** mul_u64_x_tied1:
**	mul	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mul_u64_x_tied1, svuint64_t,
		z0 = svmul_u64_x (p0, z0, z1),
		z0 = svmul_x (p0, z0, z1))

/*
** mul_u64_x_tied2:
**	mul	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mul_u64_x_tied2, svuint64_t,
		z0 = svmul_u64_x (p0, z1, z0),
		z0 = svmul_x (p0, z1, z0))

/*
** mul_u64_x_untied:
** (
**	movprfx	z0, z1
**	mul	z0\.d, p0/m, z0\.d, z2\.d
** |
**	movprfx	z0, z2
**	mul	z0\.d, p0/m, z0\.d, z1\.d
** )
**	ret
*/
TEST_UNIFORM_Z (mul_u64_x_untied, svuint64_t,
		z0 = svmul_u64_x (p0, z1, z2),
		z0 = svmul_x (p0, z1, z2))

/*
** mul_x0_u64_x_tied1:
**	mov	(z[0-9]+\.d), x0
**	mul	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_ZX (mul_x0_u64_x_tied1, svuint64_t, uint64_t,
		 z0 = svmul_n_u64_x (p0, z0, x0),
		 z0 = svmul_x (p0, z0, x0))

/*
** mul_x0_u64_x_untied:
**	mov	z0\.d, x0
**	mul	z0\.d, p0/m, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_ZX (mul_x0_u64_x_untied, svuint64_t, uint64_t,
		 z0 = svmul_n_u64_x (p0, z1, x0),
		 z0 = svmul_x (p0, z1, x0))

/*
** mul_4dupop1_u64_x_tied2:
**	lsl	z0\.d, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1_u64_x_tied2, svuint64_t,
		z0 = svmul_x (p0, svdup_u64 (4), z0),
		z0 = svmul_x (p0, svdup_u64 (4), z0))

/*
** mul_4dupop1ptrue_u64_x_tied2:
**	lsl	z0\.d, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1ptrue_u64_x_tied2, svuint64_t,
		z0 = svmul_x (svptrue_b64 (), svdup_u64 (4), z0),
		z0 = svmul_x (svptrue_b64 (), svdup_u64 (4), z0))

/*
** mul_4dupop2_u64_x_tied1:
**	lsl	z0\.d, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_u64_x_tied1, svuint64_t,
		z0 = svmul_x (p0, z0, svdup_u64 (4)),
		z0 = svmul_x (p0, z0, svdup_u64 (4)))

/*
** mul_4nop2_u64_x_tied1:
**	lsl	z0\.d, z0\.d, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_u64_x_tied1, svuint64_t,
		z0 = svmul_n_u64_x (p0, z0, 4),
		z0 = svmul_x (p0, z0, 4))

/*
** mul_maxpownop2_u64_x_tied1:
**	lsl	z0\.d, z0\.d, #63
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_u64_x_tied1, svuint64_t,
		z0 = svmul_n_u64_x (p0, z0, MAXPOW),
		z0 = svmul_x (p0, z0, MAXPOW))

/*
** mul_1_u64_x_tied1:
**	ret
*/
TEST_UNIFORM_Z (mul_1_u64_x_tied1, svuint64_t,
		z0 = svmul_n_u64_x (p0, z0, 1),
		z0 = svmul_x (p0, z0, 1))

/*
** mul_1op1_u64_x_tied2:
**	ret
*/
TEST_UNIFORM_Z (mul_1op1_u64_x_tied2, svuint64_t,
		z0 = svmul_u64_x (p0, svdup_u64 (1), z0),
		z0 = svmul_x (p0, svdup_u64 (1), z0))

/*
** mul_1op1_u64_x_untied:
**	mov	z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (mul_1op1_u64_x_untied, svuint64_t,
		z0 = svmul_u64_x (p0, svdup_u64 (1), z1),
		z0 = svmul_x (p0, svdup_u64 (1), z1))

/*
** mul_2_u64_x_tied1:
**	add	z0\.d, z0\.d, z0\.d
**	ret
*/
TEST_UNIFORM_Z (mul_2_u64_x_tied1, svuint64_t,
		z0 = svmul_n_u64_x (p0, z0, 2),
		z0 = svmul_x (p0, z0, 2))

/*
** mul_3_u64_x_tied1:
**	mul	z0\.d, z0\.d, #3
**	ret
*/
TEST_UNIFORM_Z (mul_3_u64_x_tied1, svuint64_t,
		z0 = svmul_n_u64_x (p0, z0, 3),
		z0 = svmul_x (p0, z0, 3))

/*
** mul_4dupop2_u64_x_untied:
**	lsl	z0\.d, z1\.d, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_u64_x_untied, svuint64_t,
		z0 = svmul_x (p0, z1, svdup_u64 (4)),
		z0 = svmul_x (p0, z1, svdup_u64 (4)))

/*
** mul_4nop2_u64_x_untied:
**	lsl	z0\.d, z1\.d, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_u64_x_untied, svuint64_t,
		z0 = svmul_n_u64_x (p0, z1, 4),
		z0 = svmul_x (p0, z1, 4))

/*
** mul_maxpownop2_u64_x_untied:
**	lsl	z0\.d, z1\.d, #63
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_u64_x_untied, svuint64_t,
		z0 = svmul_n_u64_x (p0, z1, MAXPOW),
		z0 = svmul_x (p0, z1, MAXPOW))

/*
** mul_3_u64_x_untied:
**	movprfx	z0, z1
**	mul	z0\.d, z0\.d, #3
**	ret
*/
TEST_UNIFORM_Z (mul_3_u64_x_untied, svuint64_t,
		z0 = svmul_n_u64_x (p0, z1, 3),
		z0 = svmul_x (p0, z1, 3))

/*
** mul_127_u64_x:
**	mul	z0\.d, z0\.d, #127
**	ret
*/
TEST_UNIFORM_Z (mul_127_u64_x, svuint64_t,
		z0 = svmul_n_u64_x (p0, z0, 127),
		z0 = svmul_x (p0, z0, 127))

/*
** mul_128_u64_x:
**	lsl	z0\.d, z0\.d, #7
**	ret
*/
TEST_UNIFORM_Z (mul_128_u64_x, svuint64_t,
		z0 = svmul_n_u64_x (p0, z0, 128),
		z0 = svmul_x (p0, z0, 128))

/*
** mul_255_u64_x:
**	mov	(z[0-9]+\.d), #255
**	mul	z0\.d, p0/m, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (mul_255_u64_x, svuint64_t,
		z0 = svmul_n_u64_x (p0, z0, 255),
		z0 = svmul_x (p0, z0, 255))

/*
** mul_m1_u64_x:
**	mul	z0\.d, z0\.d, #-1
**	ret
*/
TEST_UNIFORM_Z (mul_m1_u64_x, svuint64_t,
		z0 = svmul_n_u64_x (p0, z0, -1),
		z0 = svmul_x (p0, z0, -1))

/*
** mul_m127_u64_x:
**	mul	z0\.d, z0\.d, #-127
**	ret
*/
TEST_UNIFORM_Z (mul_m127_u64_x, svuint64_t,
		z0 = svmul_n_u64_x (p0, z0, -127),
		z0 = svmul_x (p0, z0, -127))

/*
** mul_m128_u64_x:
**	mul	z0\.d, z0\.d, #-128
**	ret
*/
TEST_UNIFORM_Z (mul_m128_u64_x, svuint64_t,
		z0 = svmul_n_u64_x (p0, z0, -128),
		z0 = svmul_x (p0, z0, -128))
