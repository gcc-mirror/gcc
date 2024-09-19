/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#define MAXPOW 1ULL<<30

/*
** mul_s32_m_tied1:
**	mul	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mul_s32_m_tied1, svint32_t,
		z0 = svmul_s32_m (p0, z0, z1),
		z0 = svmul_m (p0, z0, z1))

/*
** mul_s32_m_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	mul	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (mul_s32_m_tied2, svint32_t,
		z0 = svmul_s32_m (p0, z1, z0),
		z0 = svmul_m (p0, z1, z0))

/*
** mul_s32_m_untied:
**	movprfx	z0, z1
**	mul	z0\.s, p0/m, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (mul_s32_m_untied, svint32_t,
		z0 = svmul_s32_m (p0, z1, z2),
		z0 = svmul_m (p0, z1, z2))

/*
** mul_w0_s32_m_tied1:
**	mov	(z[0-9]+\.s), w0
**	mul	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (mul_w0_s32_m_tied1, svint32_t, int32_t,
		 z0 = svmul_n_s32_m (p0, z0, x0),
		 z0 = svmul_m (p0, z0, x0))

/*
** mul_w0_s32_m_untied:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0, z1
**	mul	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (mul_w0_s32_m_untied, svint32_t, int32_t,
		 z0 = svmul_n_s32_m (p0, z1, x0),
		 z0 = svmul_m (p0, z1, x0))

/*
** mul_4dupop1_s32_m_tied2:
**	mov	(z[0-9]+)\.s, #4
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, \1
**	mul	z0\.s, p0/m, z0\.s, \2\.s
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1_s32_m_tied2, svint32_t,
		z0 = svmul_m (p0, svdup_s32 (4), z0),
		z0 = svmul_m (p0, svdup_s32 (4), z0))

/*
** mul_4dupop1ptrue_s32_m_tied2:
**	lsl	z0\.s, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1ptrue_s32_m_tied2, svint32_t,
		z0 = svmul_m (svptrue_b32 (), svdup_s32 (4), z0),
		z0 = svmul_m (svptrue_b32 (), svdup_s32 (4), z0))

/*
** mul_4dupop2_s32_m_tied1:
**	lsl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_s32_m_tied1, svint32_t,
		z0 = svmul_m (p0, z0, svdup_s32 (4)),
		z0 = svmul_m (p0, z0, svdup_s32 (4)))

/*
** mul_4nop2_s32_m_tied1:
**	lsl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_s32_m_tied1, svint32_t,
		z0 = svmul_n_s32_m (p0, z0, 4),
		z0 = svmul_m (p0, z0, 4))

/*
** mul_maxpownop2_s32_m_tied1:
**	lsl	z0\.s, p0/m, z0\.s, #30
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_s32_m_tied1, svint32_t,
		z0 = svmul_n_s32_m (p0, z0, MAXPOW),
		z0 = svmul_m (p0, z0, MAXPOW))

/*
** mul_intminnop2_s32_m_tied1:
**	lsl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (mul_intminnop2_s32_m_tied1, svint32_t,
		z0 = svmul_n_s32_m (p0, z0, INT32_MIN),
		z0 = svmul_m (p0, z0, INT32_MIN))

/*
** mul_1_s32_m_tied1:
**	sel	z0\.s, p0, z0\.s, z0\.s
**	ret
*/
TEST_UNIFORM_Z (mul_1_s32_m_tied1, svint32_t,
		z0 = svmul_n_s32_m (p0, z0, 1),
		z0 = svmul_m (p0, z0, 1))

/*
** mul_3_s32_m_tied1:
**	mov	(z[0-9]+\.s), #3
**	mul	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (mul_3_s32_m_tied1, svint32_t,
		z0 = svmul_n_s32_m (p0, z0, 3),
		z0 = svmul_m (p0, z0, 3))

/*
** mul_4dupop2_s32_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_s32_m_untied, svint32_t,
		z0 = svmul_m (p0, z1, svdup_s32 (4)),
		z0 = svmul_m (p0, z1, svdup_s32 (4)))

/*
** mul_4nop2_s32_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_s32_m_untied, svint32_t,
		z0 = svmul_n_s32_m (p0, z1, 4),
		z0 = svmul_m (p0, z1, 4))

/*
** mul_maxpownop2_s32_m_untied:
**	movprfx	z0, z1
**	lsl	z0\.s, p0/m, z0\.s, #30
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_s32_m_untied, svint32_t,
		z0 = svmul_n_s32_m (p0, z1, MAXPOW),
		z0 = svmul_m (p0, z1, MAXPOW))

/*
** mul_3_s32_m_untied:
**	mov	(z[0-9]+\.s), #3
**	movprfx	z0, z1
**	mul	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (mul_3_s32_m_untied, svint32_t,
		z0 = svmul_n_s32_m (p0, z1, 3),
		z0 = svmul_m (p0, z1, 3))

/*
** mul_m1_s32_m:
**	mov	(z[0-9]+)\.b, #-1
**	mul	z0\.s, p0/m, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (mul_m1_s32_m, svint32_t,
		z0 = svmul_n_s32_m (p0, z0, -1),
		z0 = svmul_m (p0, z0, -1))

/*
** mul_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	mul	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mul_s32_z_tied1, svint32_t,
		z0 = svmul_s32_z (p0, z0, z1),
		z0 = svmul_z (p0, z0, z1))

/*
** mul_s32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	mul	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mul_s32_z_tied2, svint32_t,
		z0 = svmul_s32_z (p0, z1, z0),
		z0 = svmul_z (p0, z1, z0))

/*
** mul_s32_z_untied:
** (
**	movprfx	z0\.s, p0/z, z1\.s
**	mul	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0\.s, p0/z, z2\.s
**	mul	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (mul_s32_z_untied, svint32_t,
		z0 = svmul_s32_z (p0, z1, z2),
		z0 = svmul_z (p0, z1, z2))

/*
** mul_w0_s32_z_tied1:
**	mov	(z[0-9]+\.s), w0
**	movprfx	z0\.s, p0/z, z0\.s
**	mul	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (mul_w0_s32_z_tied1, svint32_t, int32_t,
		 z0 = svmul_n_s32_z (p0, z0, x0),
		 z0 = svmul_z (p0, z0, x0))

/*
** mul_w0_s32_z_untied:
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
TEST_UNIFORM_ZX (mul_w0_s32_z_untied, svint32_t, int32_t,
		 z0 = svmul_n_s32_z (p0, z1, x0),
		 z0 = svmul_z (p0, z1, x0))

/*
** mul_4dupop1_s32_z_tied2:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1_s32_z_tied2, svint32_t,
		z0 = svmul_z (p0, svdup_s32 (4), z0),
		z0 = svmul_z (p0, svdup_s32 (4), z0))

/*
** mul_4dupop1ptrue_s32_z_tied2:
**	lsl	z0\.s, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1ptrue_s32_z_tied2, svint32_t,
		z0 = svmul_z (svptrue_b32 (), svdup_s32 (4), z0),
		z0 = svmul_z (svptrue_b32 (), svdup_s32 (4), z0))

/*
** mul_4dupop2_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_s32_z_tied1, svint32_t,
		z0 = svmul_z (p0, z0, svdup_s32 (4)),
		z0 = svmul_z (p0, z0, svdup_s32 (4)))

/*
** mul_4nop2_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_s32_z_tied1, svint32_t,
		z0 = svmul_n_s32_z (p0, z0, 4),
		z0 = svmul_z (p0, z0, 4))

/*
** mul_maxpownop2_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsl	z0\.s, p0/m, z0\.s, #30
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_s32_z_tied1, svint32_t,
		z0 = svmul_n_s32_z (p0, z0, MAXPOW),
		z0 = svmul_z (p0, z0, MAXPOW))

/*
** mul_intminnop2_s32_z_tied1:
**	movprfx	z0\.s, p0/z, z0\.s
**	lsl	z0\.s, p0/m, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (mul_intminnop2_s32_z_tied1, svint32_t,
		z0 = svmul_n_s32_z (p0, z0, INT32_MIN),
		z0 = svmul_z (p0, z0, INT32_MIN))

/*
** mul_1_s32_z_tied1:
**	mov	z31.s, #1
**	movprfx	z0.s, p0/z, z0.s
**	mul	z0.s, p0/m, z0.s, z31.s
**	ret
*/
TEST_UNIFORM_Z (mul_1_s32_z_tied1, svint32_t,
		z0 = svmul_n_s32_z (p0, z0, 1),
		z0 = svmul_z (p0, z0, 1))

/*
** mul_3_s32_z_tied1:
**	mov	(z[0-9]+\.s), #3
**	movprfx	z0\.s, p0/z, z0\.s
**	mul	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (mul_3_s32_z_tied1, svint32_t,
		z0 = svmul_n_s32_z (p0, z0, 3),
		z0 = svmul_z (p0, z0, 3))

/*
** mul_4dupop2_s32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	lsl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_s32_z_untied, svint32_t,
		z0 = svmul_z (p0, z1, svdup_s32 (4)),
		z0 = svmul_z (p0, z1, svdup_s32 (4)))

/*
** mul_4nop2_s32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	lsl	z0\.s, p0/m, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_s32_z_untied, svint32_t,
		z0 = svmul_n_s32_z (p0, z1, 4),
		z0 = svmul_z (p0, z1, 4))

/*
** mul_maxpownop2_s32_z_untied:
**	movprfx	z0\.s, p0/z, z1\.s
**	lsl	z0\.s, p0/m, z0\.s, #30
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_s32_z_untied, svint32_t,
		z0 = svmul_n_s32_z (p0, z1, MAXPOW),
		z0 = svmul_z (p0, z1, MAXPOW))

/*
** mul_3_s32_z_untied:
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
TEST_UNIFORM_Z (mul_3_s32_z_untied, svint32_t,
		z0 = svmul_n_s32_z (p0, z1, 3),
		z0 = svmul_z (p0, z1, 3))

/*
** mul_s32_x_tied1:
**	mul	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mul_s32_x_tied1, svint32_t,
		z0 = svmul_s32_x (p0, z0, z1),
		z0 = svmul_x (p0, z0, z1))

/*
** mul_s32_x_tied2:
**	mul	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (mul_s32_x_tied2, svint32_t,
		z0 = svmul_s32_x (p0, z1, z0),
		z0 = svmul_x (p0, z1, z0))

/*
** mul_s32_x_untied:
** (
**	movprfx	z0, z1
**	mul	z0\.s, p0/m, z0\.s, z2\.s
** |
**	movprfx	z0, z2
**	mul	z0\.s, p0/m, z0\.s, z1\.s
** )
**	ret
*/
TEST_UNIFORM_Z (mul_s32_x_untied, svint32_t,
		z0 = svmul_s32_x (p0, z1, z2),
		z0 = svmul_x (p0, z1, z2))

/*
** mul_w0_s32_x_tied1:
**	mov	(z[0-9]+\.s), w0
**	mul	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_ZX (mul_w0_s32_x_tied1, svint32_t, int32_t,
		 z0 = svmul_n_s32_x (p0, z0, x0),
		 z0 = svmul_x (p0, z0, x0))

/*
** mul_w0_s32_x_untied:
**	mov	z0\.s, w0
**	mul	z0\.s, p0/m, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_ZX (mul_w0_s32_x_untied, svint32_t, int32_t,
		 z0 = svmul_n_s32_x (p0, z1, x0),
		 z0 = svmul_x (p0, z1, x0))

/*
** mul_4dupop1_s32_x_tied2:
**	lsl	z0\.s, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1_s32_x_tied2, svint32_t,
		z0 = svmul_x (p0, svdup_s32 (4), z0),
		z0 = svmul_x (p0, svdup_s32 (4), z0))

/*
** mul_4dupop1ptrue_s32_x_tied2:
**	lsl	z0\.s, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop1ptrue_s32_x_tied2, svint32_t,
		z0 = svmul_x (svptrue_b32 (), svdup_s32 (4), z0),
		z0 = svmul_x (svptrue_b32 (), svdup_s32 (4), z0))

/*
** mul_4dupop2_s32_x_tied1:
**	lsl	z0\.s, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_s32_x_tied1, svint32_t,
		z0 = svmul_x (p0, z0, svdup_s32 (4)),
		z0 = svmul_x (p0, z0, svdup_s32 (4)))

/*
** mul_4nop2_s32_x_tied1:
**	lsl	z0\.s, z0\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_s32_x_tied1, svint32_t,
		z0 = svmul_n_s32_x (p0, z0, 4),
		z0 = svmul_x (p0, z0, 4))

/*
** mul_maxpownop2_s32_x_tied1:
**	lsl	z0\.s, z0\.s, #30
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_s32_x_tied1, svint32_t,
		z0 = svmul_n_s32_x (p0, z0, MAXPOW),
		z0 = svmul_x (p0, z0, MAXPOW))

/*
** mul_intminnop2_s32_x_tied1:
**	lsl	z0\.s, z0\.s, #31
**	ret
*/
TEST_UNIFORM_Z (mul_intminnop2_s32_x_tied1, svint32_t,
		z0 = svmul_n_s32_x (p0, z0, INT32_MIN),
		z0 = svmul_x (p0, z0, INT32_MIN))

/*
** mul_1_s32_x_tied1:
**	ret
*/
TEST_UNIFORM_Z (mul_1_s32_x_tied1, svint32_t,
		z0 = svmul_n_s32_x (p0, z0, 1),
		z0 = svmul_x (p0, z0, 1))

/*
** mul_3_s32_x_tied1:
**	mul	z0\.s, z0\.s, #3
**	ret
*/
TEST_UNIFORM_Z (mul_3_s32_x_tied1, svint32_t,
		z0 = svmul_n_s32_x (p0, z0, 3),
		z0 = svmul_x (p0, z0, 3))

/*
** mul_4dupop2_s32_x_untied:
**	lsl	z0\.s, z1\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4dupop2_s32_x_untied, svint32_t,
		z0 = svmul_x (p0, z1, svdup_s32 (4)),
		z0 = svmul_x (p0, z1, svdup_s32 (4)))

/*
** mul_4nop2_s32_x_untied:
**	lsl	z0\.s, z1\.s, #2
**	ret
*/
TEST_UNIFORM_Z (mul_4nop2_s32_x_untied, svint32_t,
		z0 = svmul_n_s32_x (p0, z1, 4),
		z0 = svmul_x (p0, z1, 4))

/*
** mul_maxpownop2_s32_x_untied:
**	lsl	z0\.s, z1\.s, #30
**	ret
*/
TEST_UNIFORM_Z (mul_maxpownop2_s32_x_untied, svint32_t,
		z0 = svmul_n_s32_x (p0, z1, MAXPOW),
		z0 = svmul_x (p0, z1, MAXPOW))

/*
** mul_3_s32_x_untied:
**	movprfx	z0, z1
**	mul	z0\.s, z0\.s, #3
**	ret
*/
TEST_UNIFORM_Z (mul_3_s32_x_untied, svint32_t,
		z0 = svmul_n_s32_x (p0, z1, 3),
		z0 = svmul_x (p0, z1, 3))

/*
** mul_127_s32_x:
**	mul	z0\.s, z0\.s, #127
**	ret
*/
TEST_UNIFORM_Z (mul_127_s32_x, svint32_t,
		z0 = svmul_n_s32_x (p0, z0, 127),
		z0 = svmul_x (p0, z0, 127))

/*
** mul_128_s32_x:
**	lsl	z0\.s, z0\.s, #7
**	ret
*/
TEST_UNIFORM_Z (mul_128_s32_x, svint32_t,
		z0 = svmul_n_s32_x (p0, z0, 128),
		z0 = svmul_x (p0, z0, 128))

/*
** mul_255_s32_x:
**	mov	(z[0-9]+\.s), #255
**	mul	z0\.s, p0/m, z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (mul_255_s32_x, svint32_t,
		z0 = svmul_n_s32_x (p0, z0, 255),
		z0 = svmul_x (p0, z0, 255))

/*
** mul_m1_s32_x:
**	mul	z0\.s, z0\.s, #-1
**	ret
*/
TEST_UNIFORM_Z (mul_m1_s32_x, svint32_t,
		z0 = svmul_n_s32_x (p0, z0, -1),
		z0 = svmul_x (p0, z0, -1))

/*
** mul_m127_s32_x:
**	mul	z0\.s, z0\.s, #-127
**	ret
*/
TEST_UNIFORM_Z (mul_m127_s32_x, svint32_t,
		z0 = svmul_n_s32_x (p0, z0, -127),
		z0 = svmul_x (p0, z0, -127))

/*
** mul_m128_s32_x:
**	mul	z0\.s, z0\.s, #-128
**	ret
*/
TEST_UNIFORM_Z (mul_m128_s32_x, svint32_t,
		z0 = svmul_n_s32_x (p0, z0, -128),
		z0 = svmul_x (p0, z0, -128))
