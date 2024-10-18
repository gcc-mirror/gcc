/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dup_1_s8:
**	mov	z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (dup_1_s8, svint8_t,
		z0 = svdup_n_s8 (1),
		z0 = svdup_s8 (1))

/*
** dup_127_s8:
**	mov	z0\.b, #127
**	ret
*/
TEST_UNIFORM_Z (dup_127_s8, svint8_t,
		z0 = svdup_n_s8 (127),
		z0 = svdup_s8 (127))

/*
** dup_128_s8:
**	mov	z0\.b, #-128
**	ret
*/
TEST_UNIFORM_Z (dup_128_s8, svint8_t,
		z0 = svdup_n_s8 (128),
		z0 = svdup_s8 (128))

/*
** dup_129_s8:
**	mov	z0\.b, #-127
**	ret
*/
TEST_UNIFORM_Z (dup_129_s8, svint8_t,
		z0 = svdup_n_s8 (129),
		z0 = svdup_s8 (129))

/*
** dup_253_s8:
**	mov	z0\.b, #-3
**	ret
*/
TEST_UNIFORM_Z (dup_253_s8, svint8_t,
		z0 = svdup_n_s8 (253),
		z0 = svdup_s8 (253))

/*
** dup_254_s8:
**	mov	z0\.b, #-2
**	ret
*/
TEST_UNIFORM_Z (dup_254_s8, svint8_t,
		z0 = svdup_n_s8 (254),
		z0 = svdup_s8 (254))

/*
** dup_255_s8:
**	mov	z0\.b, #-1
**	ret
*/
TEST_UNIFORM_Z (dup_255_s8, svint8_t,
		z0 = svdup_n_s8 (255),
		z0 = svdup_s8 (255))

/*
** dup_m1_s8:
**	mov	z0\.b, #-1
**	ret
*/
TEST_UNIFORM_Z (dup_m1_s8, svint8_t,
		z0 = svdup_n_s8 (-1),
		z0 = svdup_s8 (-1))

/*
** dup_m128_s8:
**	mov	z0\.b, #-128
**	ret
*/
TEST_UNIFORM_Z (dup_m128_s8, svint8_t,
		z0 = svdup_n_s8 (-128),
		z0 = svdup_s8 (-128))

/*
** dup_w0_s8:
**	mov	z0\.b, w0
**	ret
*/
TEST_UNIFORM_ZX (dup_w0_s8, svint8_t, int8_t,
		 z0 = svdup_n_s8 (x0),
		 z0 = svdup_s8 (x0))

/*
** dup_1_s8_m:
**	mov	z0\.b, p0/m, #1
**	ret
*/
TEST_UNIFORM_Z (dup_1_s8_m, svint8_t,
		z0 = svdup_n_s8_m (z0, p0, 1),
		z0 = svdup_s8_m (z0, p0, 1))

/*
** dup_127_s8_m:
**	mov	z0\.b, p0/m, #127
**	ret
*/
TEST_UNIFORM_Z (dup_127_s8_m, svint8_t,
		z0 = svdup_n_s8_m (z0, p0, 127),
		z0 = svdup_s8_m (z0, p0, 127))

/*
** dup_128_s8_m:
**	mov	z0\.b, p0/m, #-128
**	ret
*/
TEST_UNIFORM_Z (dup_128_s8_m, svint8_t,
		z0 = svdup_n_s8_m (z0, p0, 128),
		z0 = svdup_s8_m (z0, p0, 128))

/*
** dup_129_s8_m:
**	mov	z0\.b, p0/m, #-127
**	ret
*/
TEST_UNIFORM_Z (dup_129_s8_m, svint8_t,
		z0 = svdup_n_s8_m (z0, p0, 129),
		z0 = svdup_s8_m (z0, p0, 129))

/*
** dup_253_s8_m:
**	mov	z0\.b, p0/m, #-3
**	ret
*/
TEST_UNIFORM_Z (dup_253_s8_m, svint8_t,
		z0 = svdup_n_s8_m (z0, p0, 253),
		z0 = svdup_s8_m (z0, p0, 253))

/*
** dup_254_s8_m:
**	mov	z0\.b, p0/m, #-2
**	ret
*/
TEST_UNIFORM_Z (dup_254_s8_m, svint8_t,
		z0 = svdup_n_s8_m (z0, p0, 254),
		z0 = svdup_s8_m (z0, p0, 254))

/*
** dup_255_s8_m:
**	mov	z0\.b, p0/m, #-1
**	ret
*/
TEST_UNIFORM_Z (dup_255_s8_m, svint8_t,
		z0 = svdup_n_s8_m (z0, p0, 255),
		z0 = svdup_s8_m (z0, p0, 255))

/*
** dup_m1_s8_m:
**	mov	z0\.b, p0/m, #-1
**	ret
*/
TEST_UNIFORM_Z (dup_m1_s8_m, svint8_t,
		z0 = svdup_n_s8_m (z0, p0, -1),
		z0 = svdup_s8_m (z0, p0, -1))

/*
** dup_m128_s8_m:
**	mov	z0\.b, p0/m, #-128
**	ret
*/
TEST_UNIFORM_Z (dup_m128_s8_m, svint8_t,
		z0 = svdup_n_s8_m (z0, p0, -128),
		z0 = svdup_s8_m (z0, p0, -128))

/*
** dup_0_s8_m:
**	mov	z0\.b, p0/m, #0
**	ret
*/
TEST_UNIFORM_Z (dup_0_s8_m, svint8_t,
		z0 = svdup_n_s8_m (z0, p0, 0),
		z0 = svdup_s8_m (z0, p0, 0))

/*
** dup_w0_s8_m:
**	movprfx	z0, z1
**	mov	z0\.b, p0/m, w0
**	ret
*/
TEST_UNIFORM_ZX (dup_w0_s8_m, svint8_t, int8_t,
		z0 = svdup_n_s8_m (z1, p0, x0),
		z0 = svdup_s8_m (z1, p0, x0))

/*
** dup_1_s8_z:
**	mov	z0\.b, p0/z, #1
**	ret
*/
TEST_UNIFORM_Z (dup_1_s8_z, svint8_t,
		z0 = svdup_n_s8_z (p0, 1),
		z0 = svdup_s8_z (p0, 1))

/*
** dup_127_s8_z:
**	mov	z0\.b, p0/z, #127
**	ret
*/
TEST_UNIFORM_Z (dup_127_s8_z, svint8_t,
		z0 = svdup_n_s8_z (p0, 127),
		z0 = svdup_s8_z (p0, 127))

/*
** dup_128_s8_z:
**	mov	z0\.b, p0/z, #-128
**	ret
*/
TEST_UNIFORM_Z (dup_128_s8_z, svint8_t,
		z0 = svdup_n_s8_z (p0, 128),
		z0 = svdup_s8_z (p0, 128))

/*
** dup_129_s8_z:
**	mov	z0\.b, p0/z, #-127
**	ret
*/
TEST_UNIFORM_Z (dup_129_s8_z, svint8_t,
		z0 = svdup_n_s8_z (p0, 129),
		z0 = svdup_s8_z (p0, 129))

/*
** dup_253_s8_z:
**	mov	z0\.b, p0/z, #-3
**	ret
*/
TEST_UNIFORM_Z (dup_253_s8_z, svint8_t,
		z0 = svdup_n_s8_z (p0, 253),
		z0 = svdup_s8_z (p0, 253))

/*
** dup_254_s8_z:
**	mov	z0\.b, p0/z, #-2
**	ret
*/
TEST_UNIFORM_Z (dup_254_s8_z, svint8_t,
		z0 = svdup_n_s8_z (p0, 254),
		z0 = svdup_s8_z (p0, 254))

/*
** dup_255_s8_z:
**	mov	z0\.b, p0/z, #-1
**	ret
*/
TEST_UNIFORM_Z (dup_255_s8_z, svint8_t,
		z0 = svdup_n_s8_z (p0, 255),
		z0 = svdup_s8_z (p0, 255))

/*
** dup_m1_s8_z:
**	mov	z0\.b, p0/z, #-1
**	ret
*/
TEST_UNIFORM_Z (dup_m1_s8_z, svint8_t,
		z0 = svdup_n_s8_z (p0, -1),
		z0 = svdup_s8_z (p0, -1))

/*
** dup_m128_s8_z:
**	mov	z0\.b, p0/z, #-128
**	ret
*/
TEST_UNIFORM_Z (dup_m128_s8_z, svint8_t,
		z0 = svdup_n_s8_z (p0, -128),
		z0 = svdup_s8_z (p0, -128))

/*
** dup_0_s8_z:
**	movi?	[vdz]0\.?(?:[0-9]*[bhsd])?, #?0
**	ret
*/
TEST_UNIFORM_Z (dup_0_s8_z, svint8_t,
		z0 = svdup_n_s8_z (p0, 0),
		z0 = svdup_s8_z (p0, 0))

/*
** dup_w0_s8_z:
**	movprfx	z0\.b, p0/z, z0\.b
**	mov	z0\.b, p0/m, w0
**	ret
*/
TEST_UNIFORM_ZX (dup_w0_s8_z, svint8_t, int8_t,
		z0 = svdup_n_s8_z (p0, x0),
		z0 = svdup_s8_z (p0, x0))

/*
** dup_1_s8_x:
**	mov	z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (dup_1_s8_x, svint8_t,
		z0 = svdup_n_s8_x (p0, 1),
		z0 = svdup_s8_x (p0, 1))

/*
** dup_127_s8_x:
**	mov	z0\.b, #127
**	ret
*/
TEST_UNIFORM_Z (dup_127_s8_x, svint8_t,
		z0 = svdup_n_s8_x (p0, 127),
		z0 = svdup_s8_x (p0, 127))

/*
** dup_128_s8_x:
**	mov	z0\.b, #-128
**	ret
*/
TEST_UNIFORM_Z (dup_128_s8_x, svint8_t,
		z0 = svdup_n_s8_x (p0, 128),
		z0 = svdup_s8_x (p0, 128))

/*
** dup_129_s8_x:
**	mov	z0\.b, #-127
**	ret
*/
TEST_UNIFORM_Z (dup_129_s8_x, svint8_t,
		z0 = svdup_n_s8_x (p0, 129),
		z0 = svdup_s8_x (p0, 129))

/*
** dup_253_s8_x:
**	mov	z0\.b, #-3
**	ret
*/
TEST_UNIFORM_Z (dup_253_s8_x, svint8_t,
		z0 = svdup_n_s8_x (p0, 253),
		z0 = svdup_s8_x (p0, 253))

/*
** dup_254_s8_x:
**	mov	z0\.b, #-2
**	ret
*/
TEST_UNIFORM_Z (dup_254_s8_x, svint8_t,
		z0 = svdup_n_s8_x (p0, 254),
		z0 = svdup_s8_x (p0, 254))

/*
** dup_255_s8_x:
**	mov	z0\.b, #-1
**	ret
*/
TEST_UNIFORM_Z (dup_255_s8_x, svint8_t,
		z0 = svdup_n_s8_x (p0, 255),
		z0 = svdup_s8_x (p0, 255))

/*
** dup_m1_s8_x:
**	mov	z0\.b, #-1
**	ret
*/
TEST_UNIFORM_Z (dup_m1_s8_x, svint8_t,
		z0 = svdup_n_s8_x (p0, -1),
		z0 = svdup_s8_x (p0, -1))

/*
** dup_m128_s8_x:
**	mov	z0\.b, #-128
**	ret
*/
TEST_UNIFORM_Z (dup_m128_s8_x, svint8_t,
		z0 = svdup_n_s8_x (p0, -128),
		z0 = svdup_s8_x (p0, -128))

/*
** dup_w0_s8_x:
**	mov	z0\.b, w0
**	ret
*/
TEST_UNIFORM_ZX (dup_w0_s8_x, svint8_t, int8_t,
		z0 = svdup_n_s8_x (p0, x0),
		z0 = svdup_s8_x (p0, x0))
