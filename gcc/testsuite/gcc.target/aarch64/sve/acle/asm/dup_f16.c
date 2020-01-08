/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dup_1_f16:
**	fmov	z0\.h, #1\.0(?:e\+0)?
**	ret
*/
TEST_UNIFORM_Z (dup_1_f16, svfloat16_t,
		z0 = svdup_n_f16 (1),
		z0 = svdup_f16 (1))

/*
** dup_0_f16:
**	mov	z0\.h, #0
**	ret
*/
TEST_UNIFORM_Z (dup_0_f16, svfloat16_t,
		z0 = svdup_n_f16 (0),
		z0 = svdup_f16 (0))

/*
** dup_8_f16:
**	fmov	z0\.h, #8\.0(?:e\+0)?
**	ret
*/
TEST_UNIFORM_Z (dup_8_f16, svfloat16_t,
		z0 = svdup_n_f16 (8),
		z0 = svdup_f16 (8))

/*
** dup_512_f16:
**	mov	z0\.h, #24576
**	ret
*/
TEST_UNIFORM_Z (dup_512_f16, svfloat16_t,
		z0 = svdup_n_f16 (512),
		z0 = svdup_f16 (512))

/*
** dup_513_f16:
**	mov	(w[0-7]+), 24578
**	mov	z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (dup_513_f16, svfloat16_t,
		z0 = svdup_n_f16 (513),
		z0 = svdup_f16 (513))

/*
** dup_h4_f16:
**	mov	z0\.h, h4
**	ret
*/
TEST_UNIFORM_ZD (dup_h4_f16, svfloat16_t, __fp16,
		z0 = svdup_n_f16 (d4),
		z0 = svdup_f16 (d4))

/*
** dup_1_f16_m:
**	mov	z0\.h, p0/m, #15360
**	ret
*/
TEST_UNIFORM_Z (dup_1_f16_m, svfloat16_t,
		z0 = svdup_n_f16_m (z0, p0, 1),
		z0 = svdup_f16_m (z0, p0, 1))

/*
** dup_0_f16_m:
**	mov	z0\.h, p0/m, #0
**	ret
*/
TEST_UNIFORM_Z (dup_0_f16_m, svfloat16_t,
		z0 = svdup_n_f16_m (z0, p0, 0),
		z0 = svdup_f16_m (z0, p0, 0))

/*
** dup_8_f16_m:
**	mov	z0\.h, p0/m, #18432
**	ret
*/
TEST_UNIFORM_Z (dup_8_f16_m, svfloat16_t,
		z0 = svdup_n_f16_m (z0, p0, 8),
		z0 = svdup_f16_m (z0, p0, 8))

/*
** dup_512_f16_m:
**	mov	z0\.h, p0/m, #24576
**	ret
*/
TEST_UNIFORM_Z (dup_512_f16_m, svfloat16_t,
		z0 = svdup_n_f16_m (z0, p0, 512),
		z0 = svdup_f16_m (z0, p0, 512))


/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_513_f16_m, svfloat16_t,
		z0 = svdup_n_f16_m (z0, p0, 513),
		z0 = svdup_f16_m (z0, p0, 513))

/*
** dup_h4_f16_m:
**	movprfx	z0, z1
**	mov	z0\.h, p0/m, h4
**	ret
*/
TEST_UNIFORM_ZD (dup_h4_f16_m, svfloat16_t, __fp16,
		z0 = svdup_n_f16_m (z1, p0, d4),
		z0 = svdup_f16_m (z1, p0, d4))

/*
** dup_1_f16_z:
**	mov	z0\.h, p0/z, #15360
**	ret
*/
TEST_UNIFORM_Z (dup_1_f16_z, svfloat16_t,
		z0 = svdup_n_f16_z (p0, 1),
		z0 = svdup_f16_z (p0, 1))

/*
** dup_0_f16_z:
**	mov	z0\.[bhsd], #0
**	ret
*/
TEST_UNIFORM_Z (dup_0_f16_z, svfloat16_t,
		z0 = svdup_n_f16_z (p0, 0),
		z0 = svdup_f16_z (p0, 0))

/*
** dup_8_f16_z:
**	mov	z0\.h, p0/z, #18432
**	ret
*/
TEST_UNIFORM_Z (dup_8_f16_z, svfloat16_t,
		z0 = svdup_n_f16_z (p0, 8),
		z0 = svdup_f16_z (p0, 8))

/*
** dup_512_f16_z:
**	mov	z0\.h, p0/z, #24576
**	ret
*/
TEST_UNIFORM_Z (dup_512_f16_z, svfloat16_t,
		z0 = svdup_n_f16_z (p0, 512),
		z0 = svdup_f16_z (p0, 512))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_513_f16_z, svfloat16_t,
		z0 = svdup_n_f16_z (p0, 513),
		z0 = svdup_f16_z (p0, 513))
/*
** dup_h4_f16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	mov	z0\.h, p0/m, h4
**	ret
*/
TEST_UNIFORM_ZD (dup_h4_f16_z, svfloat16_t, __fp16,
		z0 = svdup_n_f16_z (p0, d4),
		z0 = svdup_f16_z (p0, d4))

/*
** dup_1_f16_x:
**	fmov	z0\.h, #1\.0(?:e\+0)?
**	ret
*/
TEST_UNIFORM_Z (dup_1_f16_x, svfloat16_t,
		z0 = svdup_n_f16_x (p0, 1),
		z0 = svdup_f16_x (p0, 1))

/*
** dup_0_f16_x:
**	mov	z0\.h, #0
**	ret
*/
TEST_UNIFORM_Z (dup_0_f16_x, svfloat16_t,
		z0 = svdup_n_f16_x (p0, 0),
		z0 = svdup_f16_x (p0, 0))

/*
** dup_8_f16_x:
**	fmov	z0\.h, #8\.0(?:e\+0)?
**	ret
*/
TEST_UNIFORM_Z (dup_8_f16_x, svfloat16_t,
		z0 = svdup_n_f16_x (p0, 8),
		z0 = svdup_f16_x (p0, 8))

/*
** dup_512_f16_x:
**	mov	z0\.h, #24576
**	ret
*/
TEST_UNIFORM_Z (dup_512_f16_x, svfloat16_t,
		z0 = svdup_n_f16_x (p0, 512),
		z0 = svdup_f16_x (p0, 512))

/*
** dup_513_f16_x:
**	mov	(w[0-7]+), 24578
**	mov	z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (dup_513_f16_x, svfloat16_t,
		z0 = svdup_n_f16_x (p0, 513),
		z0 = svdup_f16_x (p0, 513))

/*
** dup_h4_f16_x:
**	mov	z0\.h, h4
**	ret
*/
TEST_UNIFORM_ZD (dup_h4_f16_x, svfloat16_t, __fp16,
		z0 = svdup_n_f16_x (p0, d4),
		z0 = svdup_f16_x (p0, d4))
