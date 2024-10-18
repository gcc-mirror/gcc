/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dup_1_f64:
**	fmov	z0\.d, #1\.0(?:e\+0)?
**	ret
*/
TEST_UNIFORM_Z (dup_1_f64, svfloat64_t,
		z0 = svdup_n_f64 (1),
		z0 = svdup_f64 (1))

/*
** dup_0_f64:
**	movi?	[vdz]0\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
TEST_UNIFORM_Z (dup_0_f64, svfloat64_t,
		z0 = svdup_n_f64 (0),
		z0 = svdup_f64 (0))

/*
** dup_8_f64:
**	fmov	z0\.d, #8\.0(?:e\+0)?
**	ret
*/
TEST_UNIFORM_Z (dup_8_f64, svfloat64_t,
		z0 = svdup_n_f64 (8),
		z0 = svdup_f64 (8))

/*
** dup_512_f64:
**	mov	(x[0-9]+), 4647714815446351872
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_512_f64, svfloat64_t,
		z0 = svdup_n_f64 (512),
		z0 = svdup_f64 (512))

/*
** dup_513_f64:
**	...
**	ld1rd	z0\.d, p[0-7]/z, \[x[0-9+]\]
**	ret
*/
TEST_UNIFORM_Z (dup_513_f64, svfloat64_t,
		z0 = svdup_n_f64 (513),
		z0 = svdup_f64 (513))

/*
** dup_d4_f64:
**	mov	z0\.d, d4
**	ret
*/
TEST_UNIFORM_ZD (dup_d4_f64, svfloat64_t, double,
		z0 = svdup_n_f64 (d4),
		z0 = svdup_f64 (d4))

/*
** dup_1_f64_m:
**	fmov	z0\.d, p0/m, #1\.0(?:e\+0)?
**	ret
*/
TEST_UNIFORM_Z (dup_1_f64_m, svfloat64_t,
		z0 = svdup_n_f64_m (z0, p0, 1),
		z0 = svdup_f64_m (z0, p0, 1))

/*
** dup_0_f64_m:
**	mov	z0\.d, p0/m, #0
**	ret
*/
TEST_UNIFORM_Z (dup_0_f64_m, svfloat64_t,
		z0 = svdup_n_f64_m (z0, p0, 0),
		z0 = svdup_f64_m (z0, p0, 0))

/*
** dup_8_f64_m:
**	fmov	z0\.d, p0/m, #8\.0(?:e\+0)?
**	ret
*/
TEST_UNIFORM_Z (dup_8_f64_m, svfloat64_t,
		z0 = svdup_n_f64_m (z0, p0, 8),
		z0 = svdup_f64_m (z0, p0, 8))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_512_f64_m, svfloat64_t,
		z0 = svdup_n_f64_m (z0, p0, 512),
		z0 = svdup_f64_m (z0, p0, 512))


/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_513_f64_m, svfloat64_t,
		z0 = svdup_n_f64_m (z0, p0, 513),
		z0 = svdup_f64_m (z0, p0, 513))

/*
** dup_d4_f64_m:
**	movprfx	z0, z1
**	mov	z0\.d, p0/m, d4
**	ret
*/
TEST_UNIFORM_ZD (dup_d4_f64_m, svfloat64_t, double,
		z0 = svdup_n_f64_m (z1, p0, d4),
		z0 = svdup_f64_m (z1, p0, d4))

/*
** dup_1_f64_z:
**	movprfx	z0\.d, p0/z, z0\.d
**	fmov	z0\.d, p0/m, #1\.0(?:e\+0)?
**	ret
*/
TEST_UNIFORM_Z (dup_1_f64_z, svfloat64_t,
		z0 = svdup_n_f64_z (p0, 1),
		z0 = svdup_f64_z (p0, 1))

/*
** dup_0_f64_z:
**	movi?	[vdz]0\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
TEST_UNIFORM_Z (dup_0_f64_z, svfloat64_t,
		z0 = svdup_n_f64_z (p0, 0),
		z0 = svdup_f64_z (p0, 0))

/*
** dup_8_f64_z:
**	movprfx	z0\.d, p0/z, z0\.d
**	fmov	z0\.d, p0/m, #8\.0(?:e\+0)?
**	ret
*/
TEST_UNIFORM_Z (dup_8_f64_z, svfloat64_t,
		z0 = svdup_n_f64_z (p0, 8),
		z0 = svdup_f64_z (p0, 8))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_512_f64_z, svfloat64_t,
		z0 = svdup_n_f64_z (p0, 512),
		z0 = svdup_f64_z (p0, 512))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_513_f64_z, svfloat64_t,
		z0 = svdup_n_f64_z (p0, 513),
		z0 = svdup_f64_z (p0, 513))

/*
** dup_d4_f64_z:
**	movprfx	z0\.d, p0/z, z0\.d
**	mov	z0\.d, p0/m, d4
**	ret
*/
TEST_UNIFORM_ZD (dup_d4_f64_z, svfloat64_t, double,
		z0 = svdup_n_f64_z (p0, d4),
		z0 = svdup_f64_z (p0, d4))

/*
** dup_1_f64_x:
**	fmov	z0\.d, #1\.0(?:e\+0)?
**	ret
*/
TEST_UNIFORM_Z (dup_1_f64_x, svfloat64_t,
		z0 = svdup_n_f64_x (p0, 1),
		z0 = svdup_f64_x (p0, 1))

/*
** dup_0_f64_x:
**	movi?	[vdz]0\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
TEST_UNIFORM_Z (dup_0_f64_x, svfloat64_t,
		z0 = svdup_n_f64_x (p0, 0),
		z0 = svdup_f64_x (p0, 0))

/*
** dup_8_f64_x:
**	fmov	z0\.d, #8\.0(?:e\+0)?
**	ret
*/
TEST_UNIFORM_Z (dup_8_f64_x, svfloat64_t,
		z0 = svdup_n_f64_x (p0, 8),
		z0 = svdup_f64_x (p0, 8))

/*
** dup_512_f64_x:
**	mov	(x[0-9]+), 4647714815446351872
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_512_f64_x, svfloat64_t,
		z0 = svdup_n_f64_x (p0, 512),
		z0 = svdup_f64_x (p0, 512))

/*
** dup_513_f64_x:
**	...
**	ld1rd	z0\.d, p[0-7]/z, \[x[0-9+]\]
**	ret
*/
TEST_UNIFORM_Z (dup_513_f64_x, svfloat64_t,
		z0 = svdup_n_f64_x (p0, 513),
		z0 = svdup_f64_x (p0, 513))

/*
** dup_d4_f64_x:
**	mov	z0\.d, d4
**	ret
*/
TEST_UNIFORM_ZD (dup_d4_f64_x, svfloat64_t, double,
		z0 = svdup_n_f64_x (p0, d4),
		z0 = svdup_f64_x (p0, d4))
