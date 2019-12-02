/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dup_1_f32:
**	fmov	z0\.s, #1\.0(?:e\+0)?
**	ret
*/
TEST_UNIFORM_Z (dup_1_f32, svfloat32_t,
		z0 = svdup_n_f32 (1),
		z0 = svdup_f32 (1))

/*
** dup_0_f32:
**	mov	z0\.s, #0
**	ret
*/
TEST_UNIFORM_Z (dup_0_f32, svfloat32_t,
		z0 = svdup_n_f32 (0),
		z0 = svdup_f32 (0))

/*
** dup_8_f32:
**	fmov	z0\.s, #8\.0(?:e\+0)?
**	ret
*/
TEST_UNIFORM_Z (dup_8_f32, svfloat32_t,
		z0 = svdup_n_f32 (8),
		z0 = svdup_f32 (8))

/*
** dup_512_f32:
**	movi	v([0-9]+).4s, 0x44, lsl 24
**	dup	z0\.q, z0\.q\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_512_f32, svfloat32_t,
		z0 = svdup_n_f32 (512),
		z0 = svdup_f32 (512))

/*
** dup_513_f32:
**	...
**	ld1rw	z0\.s, p[0-7]/z, \[x[0-9]+\]
**	ret
*/
TEST_UNIFORM_Z (dup_513_f32, svfloat32_t,
		z0 = svdup_n_f32 (513),
		z0 = svdup_f32 (513))

/*
** dup_s4_f32:
**	mov	z0\.s, s4
**	ret
*/
TEST_UNIFORM_ZD (dup_s4_f32, svfloat32_t, float,
		z0 = svdup_n_f32 (d4),
		z0 = svdup_f32 (d4))

/*
** dup_1_f32_m:
**	fmov	z0\.s, p0/m, #1\.0(?:e\+0)?
**	ret
*/
TEST_UNIFORM_Z (dup_1_f32_m, svfloat32_t,
		z0 = svdup_n_f32_m (z0, p0, 1),
		z0 = svdup_f32_m (z0, p0, 1))

/*
** dup_0_f32_m:
**	mov	z0\.s, p0/m, #0
**	ret
*/
TEST_UNIFORM_Z (dup_0_f32_m, svfloat32_t,
		z0 = svdup_n_f32_m (z0, p0, 0),
		z0 = svdup_f32_m (z0, p0, 0))

/*
** dup_8_f32_m:
**	fmov	z0\.s, p0/m, #8\.0(?:e\+0)?
**	ret
*/
TEST_UNIFORM_Z (dup_8_f32_m, svfloat32_t,
		z0 = svdup_n_f32_m (z0, p0, 8),
		z0 = svdup_f32_m (z0, p0, 8))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_512_f32_m, svfloat32_t,
		z0 = svdup_n_f32_m (z0, p0, 512),
		z0 = svdup_f32_m (z0, p0, 512))


/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_513_f32_m, svfloat32_t,
		z0 = svdup_n_f32_m (z0, p0, 513),
		z0 = svdup_f32_m (z0, p0, 513))

/*
** dup_s4_f32_m:
**	movprfx	z0, z1
**	mov	z0\.s, p0/m, s4
**	ret
*/
TEST_UNIFORM_ZD (dup_s4_f32_m, svfloat32_t, float,
		z0 = svdup_n_f32_m (z1, p0, d4),
		z0 = svdup_f32_m (z1, p0, d4))

/*
** dup_1_f32_z:
**	movprfx	z0\.s, p0/z, z0\.s
**	fmov	z0\.s, p0/m, #1\.0(?:e\+0)?
**	ret
*/
TEST_UNIFORM_Z (dup_1_f32_z, svfloat32_t,
		z0 = svdup_n_f32_z (p0, 1),
		z0 = svdup_f32_z (p0, 1))

/*
** dup_0_f32_z:
**	mov	z0\.[bhsd], #0
**	ret
*/
TEST_UNIFORM_Z (dup_0_f32_z, svfloat32_t,
		z0 = svdup_n_f32_z (p0, 0),
		z0 = svdup_f32_z (p0, 0))

/*
** dup_8_f32_z:
**	movprfx	z0\.s, p0/z, z0\.s
**	fmov	z0\.s, p0/m, #8\.0(?:e\+0)?
**	ret
*/
TEST_UNIFORM_Z (dup_8_f32_z, svfloat32_t,
		z0 = svdup_n_f32_z (p0, 8),
		z0 = svdup_f32_z (p0, 8))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_512_f32_z, svfloat32_t,
		z0 = svdup_n_f32_z (p0, 512),
		z0 = svdup_f32_z (p0, 512))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_513_f32_z, svfloat32_t,
		z0 = svdup_n_f32_z (p0, 513),
		z0 = svdup_f32_z (p0, 513))

/*
** dup_s4_f32_z:
**	movprfx	z0\.s, p0/z, z0\.s
**	mov	z0\.s, p0/m, s4
**	ret
*/
TEST_UNIFORM_ZD (dup_s4_f32_z, svfloat32_t, float,
		z0 = svdup_n_f32_z (p0, d4),
		z0 = svdup_f32_z (p0, d4))

/*
** dup_1_f32_x:
**	fmov	z0\.s, #1\.0(?:e\+0)?
**	ret
*/
TEST_UNIFORM_Z (dup_1_f32_x, svfloat32_t,
		z0 = svdup_n_f32_x (p0, 1),
		z0 = svdup_f32_x (p0, 1))

/*
** dup_0_f32_x:
**	mov	z0\.s, #0
**	ret
*/
TEST_UNIFORM_Z (dup_0_f32_x, svfloat32_t,
		z0 = svdup_n_f32_x (p0, 0),
		z0 = svdup_f32_x (p0, 0))

/*
** dup_8_f32_x:
**	fmov	z0\.s, #8\.0(?:e\+0)?
**	ret
*/
TEST_UNIFORM_Z (dup_8_f32_x, svfloat32_t,
		z0 = svdup_n_f32_x (p0, 8),
		z0 = svdup_f32_x (p0, 8))

/*
** dup_512_f32_x:
**	movi	v([0-9]+).4s, 0x44, lsl 24
**	dup	z0\.q, z0\.q\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_512_f32_x, svfloat32_t,
		z0 = svdup_n_f32_x (p0, 512),
		z0 = svdup_f32_x (p0, 512))

/*
** dup_513_f32_x:
**	...
**	ld1rw	z0\.s, p[0-7]/z, \[x[0-9]+\]
**	ret
*/
TEST_UNIFORM_Z (dup_513_f32_x, svfloat32_t,
		z0 = svdup_n_f32_x (p0, 513),
		z0 = svdup_f32_x (p0, 513))

/*
** dup_s4_f32_x:
**	mov	z0\.s, s4
**	ret
*/
TEST_UNIFORM_ZD (dup_s4_f32_x, svfloat32_t, float,
		z0 = svdup_n_f32_x (p0, d4),
		z0 = svdup_f32_x (p0, d4))
