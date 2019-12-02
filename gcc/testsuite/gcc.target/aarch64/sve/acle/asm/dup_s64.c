/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dup_1_s64:
**	mov	z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (dup_1_s64, svint64_t,
		z0 = svdup_n_s64 (1),
		z0 = svdup_s64 (1))

/*
** dup_127_s64:
**	mov	z0\.d, #127
**	ret
*/
TEST_UNIFORM_Z (dup_127_s64, svint64_t,
		z0 = svdup_n_s64 (127),
		z0 = svdup_s64 (127))

/*
** dup_128_s64:
**	mov	z0\.d, #128
**	ret
*/
TEST_UNIFORM_Z (dup_128_s64, svint64_t,
		z0 = svdup_n_s64 (128),
		z0 = svdup_s64 (128))

/*
** dup_129_s64:
**	mov	(x[0-9]+), 129
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_129_s64, svint64_t,
		z0 = svdup_n_s64 (129),
		z0 = svdup_s64 (129))

/*
** dup_253_s64:
**	mov	(x[0-9]+), 253
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_253_s64, svint64_t,
		z0 = svdup_n_s64 (253),
		z0 = svdup_s64 (253))

/*
** dup_254_s64:
**	mov	z0\.d, #254
**	ret
*/
TEST_UNIFORM_Z (dup_254_s64, svint64_t,
		z0 = svdup_n_s64 (254),
		z0 = svdup_s64 (254))

/*
** dup_255_s64:
**	mov	z0\.d, #255
**	ret
*/
TEST_UNIFORM_Z (dup_255_s64, svint64_t,
		z0 = svdup_n_s64 (255),
		z0 = svdup_s64 (255))

/*
** dup_256_s64:
**	mov	z0\.d, #256
**	ret
*/
TEST_UNIFORM_Z (dup_256_s64, svint64_t,
		z0 = svdup_n_s64 (256),
		z0 = svdup_s64 (256))

/*
** dup_257_s64:
**	mov	(x[0-9]+), 257
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_257_s64, svint64_t,
		z0 = svdup_n_s64 (257),
		z0 = svdup_s64 (257))

/*
** dup_512_s64:
**	mov	z0\.d, #512
**	ret
*/
TEST_UNIFORM_Z (dup_512_s64, svint64_t,
		z0 = svdup_n_s64 (512),
		z0 = svdup_s64 (512))

/*
** dup_7f00_s64:
**	mov	z0\.d, #32512
**	ret
*/
TEST_UNIFORM_Z (dup_7f00_s64, svint64_t,
		z0 = svdup_n_s64 (0x7f00),
		z0 = svdup_s64 (0x7f00))

/*
** dup_7f01_s64:
**	mov	(x[0-9]+), 32513
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_7f01_s64, svint64_t,
		z0 = svdup_n_s64 (0x7f01),
		z0 = svdup_s64 (0x7f01))

/*
** dup_7ffd_s64:
**	mov	(x[0-9]+), 32765
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_7ffd_s64, svint64_t,
		z0 = svdup_n_s64 (0x7ffd),
		z0 = svdup_s64 (0x7ffd))

/*
** dup_7ffe_s64:
**	mov	z0\.d, #32766
**	ret
*/
TEST_UNIFORM_Z (dup_7ffe_s64, svint64_t,
		z0 = svdup_n_s64 (0x7ffe),
		z0 = svdup_s64 (0x7ffe))

/*
** dup_7fff_s64:
**	mov	z0\.d, #32767
**	ret
*/
TEST_UNIFORM_Z (dup_7fff_s64, svint64_t,
		z0 = svdup_n_s64 (0x7fff),
		z0 = svdup_s64 (0x7fff))

/*
** dup_m1_s64:
**	mov	z0\.b, #-1
**	ret
*/
TEST_UNIFORM_Z (dup_m1_s64, svint64_t,
		z0 = svdup_n_s64 (-1),
		z0 = svdup_s64 (-1))

/*
** dup_m128_s64:
**	mov	z0\.d, #-128
**	ret
*/
TEST_UNIFORM_Z (dup_m128_s64, svint64_t,
		z0 = svdup_n_s64 (-128),
		z0 = svdup_s64 (-128))

/*
** dup_m129_s64:
**	mov	z0\.d, #-129
**	ret
*/
TEST_UNIFORM_Z (dup_m129_s64, svint64_t,
		z0 = svdup_n_s64 (-129),
		z0 = svdup_s64 (-129))

/*
** dup_m130_s64:
**	mov	(x[0-9]+), -130
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m130_s64, svint64_t,
		z0 = svdup_n_s64 (-130),
		z0 = svdup_s64 (-130))

/*
** dup_m254_s64:
**	mov	(x[0-9]+), -254
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m254_s64, svint64_t,
		z0 = svdup_n_s64 (-254),
		z0 = svdup_s64 (-254))

/*
** dup_m255_s64:
**	mov	z0\.d, #-255
**	ret
*/
TEST_UNIFORM_Z (dup_m255_s64, svint64_t,
		z0 = svdup_n_s64 (-255),
		z0 = svdup_s64 (-255))

/*
** dup_m256_s64:
**	mov	z0\.d, #-256
**	ret
*/
TEST_UNIFORM_Z (dup_m256_s64, svint64_t,
		z0 = svdup_n_s64 (-256),
		z0 = svdup_s64 (-256))

/*
** dup_m257_s64:
**	mov	z0\.d, #-257
**	ret
*/
TEST_UNIFORM_Z (dup_m257_s64, svint64_t,
		z0 = svdup_n_s64 (-257),
		z0 = svdup_s64 (-257))

/*
** dup_m258_s64:
**	mov	(x[0-9]+), -258
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m258_s64, svint64_t,
		z0 = svdup_n_s64 (-258),
		z0 = svdup_s64 (-258))

/*
** dup_m259_s64:
**	mov	(x[0-9]+), -259
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m259_s64, svint64_t,
		z0 = svdup_n_s64 (-259),
		z0 = svdup_s64 (-259))

/*
** dup_m512_s64:
**	mov	z0\.d, #-512
**	ret
*/
TEST_UNIFORM_Z (dup_m512_s64, svint64_t,
		z0 = svdup_n_s64 (-512),
		z0 = svdup_s64 (-512))

/*
** dup_m7f00_s64:
**	mov	z0\.d, #-32512
**	ret
*/
TEST_UNIFORM_Z (dup_m7f00_s64, svint64_t,
		z0 = svdup_n_s64 (-0x7f00),
		z0 = svdup_s64 (-0x7f00))

/*
** dup_m7f01_s64:
**	mov	z0\.d, #-32513
**	ret
*/
TEST_UNIFORM_Z (dup_m7f01_s64, svint64_t,
		z0 = svdup_n_s64 (-0x7f01),
		z0 = svdup_s64 (-0x7f01))

/*
** dup_m7f02_s64:
**	mov	(x[0-9]+), -32514
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m7f02_s64, svint64_t,
		z0 = svdup_n_s64 (-0x7f02),
		z0 = svdup_s64 (-0x7f02))

/*
** dup_m7ffe_s64:
**	mov	(x[0-9]+), -32766
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m7ffe_s64, svint64_t,
		z0 = svdup_n_s64 (-0x7ffe),
		z0 = svdup_s64 (-0x7ffe))

/*
** dup_m7fff_s64:
**	mov	z0\.d, #-32767
**	ret
*/
TEST_UNIFORM_Z (dup_m7fff_s64, svint64_t,
		z0 = svdup_n_s64 (-0x7fff),
		z0 = svdup_s64 (-0x7fff))

/*
** dup_m8000_s64:
**	mov	z0\.d, #-32768
**	ret
*/
TEST_UNIFORM_Z (dup_m8000_s64, svint64_t,
		z0 = svdup_n_s64 (-0x8000),
		z0 = svdup_s64 (-0x8000))

/*
** dup_x0_s64:
**	mov	z0\.d, x0
**	ret
*/
TEST_UNIFORM_ZX (dup_x0_s64, svint64_t, int64_t,
		 z0 = svdup_n_s64 (x0),
		 z0 = svdup_s64 (x0))

/*
** dup_1_s64_m:
**	mov	z0\.d, p0/m, #1
**	ret
*/
TEST_UNIFORM_Z (dup_1_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, 1),
		z0 = svdup_s64_m (z0, p0, 1))

/*
** dup_127_s64_m:
**	mov	z0\.d, p0/m, #127
**	ret
*/
TEST_UNIFORM_Z (dup_127_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, 127),
		z0 = svdup_s64_m (z0, p0, 127))

/*
** dup_128_s64_m:
**	mov	(z[0-9]+\.d), #128
**	sel	z0\.d, p0, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (dup_128_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, 128),
		z0 = svdup_s64_m (z0, p0, 128))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_129_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, 129),
		z0 = svdup_s64_m (z0, p0, 129))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_253_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, 253),
		z0 = svdup_s64_m (z0, p0, 253))

/*
** dup_254_s64_m:
**	mov	(z[0-9]+\.d), #254
**	sel	z0\.d, p0, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (dup_254_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, 254),
		z0 = svdup_s64_m (z0, p0, 254))

/*
** dup_255_s64_m:
**	mov	(z[0-9]+\.d), #255
**	sel	z0\.d, p0, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (dup_255_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, 255),
		z0 = svdup_s64_m (z0, p0, 255))

/*
** dup_256_s64_m:
**	mov	z0\.d, p0/m, #256
**	ret
*/
TEST_UNIFORM_Z (dup_256_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, 256),
		z0 = svdup_s64_m (z0, p0, 256))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_257_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, 257),
		z0 = svdup_s64_m (z0, p0, 257))

/*
** dup_512_s64_m:
**	mov	z0\.d, p0/m, #512
**	ret
*/
TEST_UNIFORM_Z (dup_512_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, 512),
		z0 = svdup_s64_m (z0, p0, 512))

/*
** dup_7f00_s64_m:
**	mov	z0\.d, p0/m, #32512
**	ret
*/
TEST_UNIFORM_Z (dup_7f00_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, 0x7f00),
		z0 = svdup_s64_m (z0, p0, 0x7f00))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_7f01_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, 0x7f01),
		z0 = svdup_s64_m (z0, p0, 0x7f01))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_7ffd_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, 0x7ffd),
		z0 = svdup_s64_m (z0, p0, 0x7ffd))

/*
** dup_7ffe_s64_m:
**	mov	(z[0-9]+\.d), #32766
**	sel	z0\.d, p0, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (dup_7ffe_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, 0x7ffe),
		z0 = svdup_s64_m (z0, p0, 0x7ffe))

/*
** dup_7fff_s64_m:
**	mov	(z[0-9]+\.d), #32767
**	sel	z0\.d, p0, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (dup_7fff_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, 0x7fff),
		z0 = svdup_s64_m (z0, p0, 0x7fff))

/*
** dup_m1_s64_m:
**	mov	z0\.d, p0/m, #-1
**	ret
*/
TEST_UNIFORM_Z (dup_m1_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, -1),
		z0 = svdup_s64_m (z0, p0, -1))

/*
** dup_m128_s64_m:
**	mov	z0\.d, p0/m, #-128
**	ret
*/
TEST_UNIFORM_Z (dup_m128_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, -128),
		z0 = svdup_s64_m (z0, p0, -128))

/*
** dup_m129_s64_m:
**	mov	(z[0-9]+\.d), #-129
**	sel	z0\.d, p0, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (dup_m129_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, -129),
		z0 = svdup_s64_m (z0, p0, -129))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m130_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, -130),
		z0 = svdup_s64_m (z0, p0, -130))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m254_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, -254),
		z0 = svdup_s64_m (z0, p0, -254))

/*
** dup_m255_s64_m:
**	mov	(z[0-9]+\.d), #-255
**	sel	z0\.d, p0, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (dup_m255_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, -255),
		z0 = svdup_s64_m (z0, p0, -255))

/*
** dup_m256_s64_m:
**	mov	z0\.d, p0/m, #-256
**	ret
*/
TEST_UNIFORM_Z (dup_m256_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, -256),
		z0 = svdup_s64_m (z0, p0, -256))

/*
** dup_m257_s64_m:
**	mov	(z[0-9]+\.d), #-257
**	sel	z0\.d, p0, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (dup_m257_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, -257),
		z0 = svdup_s64_m (z0, p0, -257))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m258_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, -258),
		z0 = svdup_s64_m (z0, p0, -258))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m259_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, -259),
		z0 = svdup_s64_m (z0, p0, -259))

/*
** dup_m512_s64_m:
**	mov	z0\.d, p0/m, #-512
**	ret
*/
TEST_UNIFORM_Z (dup_m512_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, -512),
		z0 = svdup_s64_m (z0, p0, -512))

/*
** dup_m7f00_s64_m:
**	mov	z0\.d, p0/m, #-32512
**	ret
*/
TEST_UNIFORM_Z (dup_m7f00_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, -0x7f00),
		z0 = svdup_s64_m (z0, p0, -0x7f00))

/*
** dup_m7f01_s64_m:
**	mov	(z[0-9]+\.d), #-32513
**	sel	z0\.d, p0, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (dup_m7f01_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, -0x7f01),
		z0 = svdup_s64_m (z0, p0, -0x7f01))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m7f02_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, -0x7f02),
		z0 = svdup_s64_m (z0, p0, -0x7f02))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m7ffe_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, -0x7ffe),
		z0 = svdup_s64_m (z0, p0, -0x7ffe))

/*
** dup_m7fff_s64_m:
**	mov	(z[0-9]+\.d), #-32767
**	sel	z0\.d, p0, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (dup_m7fff_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, -0x7fff),
		z0 = svdup_s64_m (z0, p0, -0x7fff))

/*
** dup_m8000_s64_m:
**	mov	z0\.d, p0/m, #-32768
**	ret
*/
TEST_UNIFORM_Z (dup_m8000_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, -0x8000),
		z0 = svdup_s64_m (z0, p0, -0x8000))

/*
** dup_0_s64_m:
**	mov	z0\.d, p0/m, #0
**	ret
*/
TEST_UNIFORM_Z (dup_0_s64_m, svint64_t,
		z0 = svdup_n_s64_m (z0, p0, 0),
		z0 = svdup_s64_m (z0, p0, 0))

/*
** dup_x0_s64_m:
**	movprfx	z0, z1
**	mov	z0\.d, p0/m, x0
**	ret
*/
TEST_UNIFORM_ZX (dup_x0_s64_m, svint64_t, int64_t,
		z0 = svdup_n_s64_m (z1, p0, x0),
		z0 = svdup_s64_m (z1, p0, x0))

/*
** dup_1_s64_z:
**	mov	z0\.d, p0/z, #1
**	ret
*/
TEST_UNIFORM_Z (dup_1_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, 1),
		z0 = svdup_s64_z (p0, 1))

/*
** dup_127_s64_z:
**	mov	z0\.d, p0/z, #127
**	ret
*/
TEST_UNIFORM_Z (dup_127_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, 127),
		z0 = svdup_s64_z (p0, 127))

/*
** dup_128_s64_z:
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.d), #128
**	sel	z0\.d, p0, \2, \1\.d
**	ret
*/
TEST_UNIFORM_Z (dup_128_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, 128),
		z0 = svdup_s64_z (p0, 128))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_129_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, 129),
		z0 = svdup_s64_z (p0, 129))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_253_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, 253),
		z0 = svdup_s64_z (p0, 253))

/*
** dup_254_s64_z:
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.d), #254
**	sel	z0\.d, p0, \2, \1\.d
**	ret
*/
TEST_UNIFORM_Z (dup_254_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, 254),
		z0 = svdup_s64_z (p0, 254))

/*
** dup_255_s64_z:
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.d), #255
**	sel	z0\.d, p0, \2, \1\.d
**	ret
*/
TEST_UNIFORM_Z (dup_255_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, 255),
		z0 = svdup_s64_z (p0, 255))

/*
** dup_256_s64_z:
**	mov	z0\.d, p0/z, #256
**	ret
*/
TEST_UNIFORM_Z (dup_256_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, 256),
		z0 = svdup_s64_z (p0, 256))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_257_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, 257),
		z0 = svdup_s64_z (p0, 257))

/*
** dup_512_s64_z:
**	mov	z0\.d, p0/z, #512
**	ret
*/
TEST_UNIFORM_Z (dup_512_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, 512),
		z0 = svdup_s64_z (p0, 512))

/*
** dup_7f00_s64_z:
**	mov	z0\.d, p0/z, #32512
**	ret
*/
TEST_UNIFORM_Z (dup_7f00_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, 0x7f00),
		z0 = svdup_s64_z (p0, 0x7f00))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_7f01_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, 0x7f01),
		z0 = svdup_s64_z (p0, 0x7f01))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_7ffd_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, 0x7ffd),
		z0 = svdup_s64_z (p0, 0x7ffd))

/*
** dup_7ffe_s64_z:
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.d), #32766
**	sel	z0\.d, p0, \2, \1\.d
**	ret
*/
TEST_UNIFORM_Z (dup_7ffe_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, 0x7ffe),
		z0 = svdup_s64_z (p0, 0x7ffe))

/*
** dup_7fff_s64_z:
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.d), #32767
**	sel	z0\.d, p0, \2, \1\.d
**	ret
*/
TEST_UNIFORM_Z (dup_7fff_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, 0x7fff),
		z0 = svdup_s64_z (p0, 0x7fff))

/*
** dup_m1_s64_z:
**	mov	z0\.d, p0/z, #-1
**	ret
*/
TEST_UNIFORM_Z (dup_m1_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, -1),
		z0 = svdup_s64_z (p0, -1))

/*
** dup_m128_s64_z:
**	mov	z0\.d, p0/z, #-128
**	ret
*/
TEST_UNIFORM_Z (dup_m128_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, -128),
		z0 = svdup_s64_z (p0, -128))

/*
** dup_m129_s64_z:
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.d), #-129
**	sel	z0\.d, p0, \2, \1\.d
**	ret
*/
TEST_UNIFORM_Z (dup_m129_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, -129),
		z0 = svdup_s64_z (p0, -129))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m130_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, -130),
		z0 = svdup_s64_z (p0, -130))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m254_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, -254),
		z0 = svdup_s64_z (p0, -254))

/*
** dup_m255_s64_z:
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.d), #-255
**	sel	z0\.d, p0, \2, \1\.d
**	ret
*/
TEST_UNIFORM_Z (dup_m255_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, -255),
		z0 = svdup_s64_z (p0, -255))

/*
** dup_m256_s64_z:
**	mov	z0\.d, p0/z, #-256
**	ret
*/
TEST_UNIFORM_Z (dup_m256_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, -256),
		z0 = svdup_s64_z (p0, -256))

/*
** dup_m257_s64_z:
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.d), #-257
**	sel	z0\.d, p0, \2, \1\.d
**	ret
*/
TEST_UNIFORM_Z (dup_m257_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, -257),
		z0 = svdup_s64_z (p0, -257))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m258_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, -258),
		z0 = svdup_s64_z (p0, -258))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m259_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, -259),
		z0 = svdup_s64_z (p0, -259))

/*
** dup_m512_s64_z:
**	mov	z0\.d, p0/z, #-512
**	ret
*/
TEST_UNIFORM_Z (dup_m512_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, -512),
		z0 = svdup_s64_z (p0, -512))

/*
** dup_m7f00_s64_z:
**	mov	z0\.d, p0/z, #-32512
**	ret
*/
TEST_UNIFORM_Z (dup_m7f00_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, -0x7f00),
		z0 = svdup_s64_z (p0, -0x7f00))

/*
** dup_m7f01_s64_z:
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.d), #-32513
**	sel	z0\.d, p0, \2, \1\.d
**	ret
*/
TEST_UNIFORM_Z (dup_m7f01_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, -0x7f01),
		z0 = svdup_s64_z (p0, -0x7f01))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m7f02_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, -0x7f02),
		z0 = svdup_s64_z (p0, -0x7f02))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m7ffe_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, -0x7ffe),
		z0 = svdup_s64_z (p0, -0x7ffe))

/*
** dup_m7fff_s64_z:
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.d), #-32767
**	sel	z0\.d, p0, \2, \1\.d
**	ret
*/
TEST_UNIFORM_Z (dup_m7fff_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, -0x7fff),
		z0 = svdup_s64_z (p0, -0x7fff))

/*
** dup_m8000_s64_z:
**	mov	z0\.d, p0/z, #-32768
**	ret
*/
TEST_UNIFORM_Z (dup_m8000_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, -0x8000),
		z0 = svdup_s64_z (p0, -0x8000))

/*
** dup_0_s64_z:
**	mov	z0\.[bhsd], #0
**	ret
*/
TEST_UNIFORM_Z (dup_0_s64_z, svint64_t,
		z0 = svdup_n_s64_z (p0, 0),
		z0 = svdup_s64_z (p0, 0))

/*
** dup_x0_s64_z:
**	movprfx	z0\.d, p0/z, z0\.d
**	mov	z0\.d, p0/m, x0
**	ret
*/
TEST_UNIFORM_ZX (dup_x0_s64_z, svint64_t, int64_t,
		z0 = svdup_n_s64_z (p0, x0),
		z0 = svdup_s64_z (p0, x0))

/*
** dup_1_s64_x:
**	mov	z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (dup_1_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, 1),
		z0 = svdup_s64_x (p0, 1))

/*
** dup_127_s64_x:
**	mov	z0\.d, #127
**	ret
*/
TEST_UNIFORM_Z (dup_127_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, 127),
		z0 = svdup_s64_x (p0, 127))

/*
** dup_128_s64_x:
**	mov	z0\.d, #128
**	ret
*/
TEST_UNIFORM_Z (dup_128_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, 128),
		z0 = svdup_s64_x (p0, 128))

/*
** dup_129_s64_x:
**	mov	(x[0-9]+), 129
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_129_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, 129),
		z0 = svdup_s64_x (p0, 129))

/*
** dup_253_s64_x:
**	mov	(x[0-9]+), 253
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_253_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, 253),
		z0 = svdup_s64_x (p0, 253))

/*
** dup_254_s64_x:
**	mov	z0\.d, #254
**	ret
*/
TEST_UNIFORM_Z (dup_254_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, 254),
		z0 = svdup_s64_x (p0, 254))

/*
** dup_255_s64_x:
**	mov	z0\.d, #255
**	ret
*/
TEST_UNIFORM_Z (dup_255_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, 255),
		z0 = svdup_s64_x (p0, 255))

/*
** dup_256_s64_x:
**	mov	z0\.d, #256
**	ret
*/
TEST_UNIFORM_Z (dup_256_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, 256),
		z0 = svdup_s64_x (p0, 256))

/*
** dup_257_s64_x:
**	mov	(x[0-9]+), 257
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_257_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, 257),
		z0 = svdup_s64_x (p0, 257))

/*
** dup_512_s64_x:
**	mov	z0\.d, #512
**	ret
*/
TEST_UNIFORM_Z (dup_512_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, 512),
		z0 = svdup_s64_x (p0, 512))

/*
** dup_7f00_s64_x:
**	mov	z0\.d, #32512
**	ret
*/
TEST_UNIFORM_Z (dup_7f00_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, 0x7f00),
		z0 = svdup_s64_x (p0, 0x7f00))

/*
** dup_7f01_s64_x:
**	mov	(x[0-9]+), 32513
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_7f01_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, 0x7f01),
		z0 = svdup_s64_x (p0, 0x7f01))

/*
** dup_7ffd_s64_x:
**	mov	(x[0-9]+), 32765
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_7ffd_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, 0x7ffd),
		z0 = svdup_s64_x (p0, 0x7ffd))

/*
** dup_7ffe_s64_x:
**	mov	z0\.d, #32766
**	ret
*/
TEST_UNIFORM_Z (dup_7ffe_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, 0x7ffe),
		z0 = svdup_s64_x (p0, 0x7ffe))

/*
** dup_7fff_s64_x:
**	mov	z0\.d, #32767
**	ret
*/
TEST_UNIFORM_Z (dup_7fff_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, 0x7fff),
		z0 = svdup_s64_x (p0, 0x7fff))

/*
** dup_m1_s64_x:
**	mov	z0\.b, #-1
**	ret
*/
TEST_UNIFORM_Z (dup_m1_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, -1),
		z0 = svdup_s64_x (p0, -1))

/*
** dup_m128_s64_x:
**	mov	z0\.d, #-128
**	ret
*/
TEST_UNIFORM_Z (dup_m128_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, -128),
		z0 = svdup_s64_x (p0, -128))

/*
** dup_m129_s64_x:
**	mov	z0\.d, #-129
**	ret
*/
TEST_UNIFORM_Z (dup_m129_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, -129),
		z0 = svdup_s64_x (p0, -129))

/*
** dup_m130_s64_x:
**	mov	(x[0-9]+), -130
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m130_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, -130),
		z0 = svdup_s64_x (p0, -130))

/*
** dup_m254_s64_x:
**	mov	(x[0-9]+), -254
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m254_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, -254),
		z0 = svdup_s64_x (p0, -254))

/*
** dup_m255_s64_x:
**	mov	z0\.d, #-255
**	ret
*/
TEST_UNIFORM_Z (dup_m255_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, -255),
		z0 = svdup_s64_x (p0, -255))

/*
** dup_m256_s64_x:
**	mov	z0\.d, #-256
**	ret
*/
TEST_UNIFORM_Z (dup_m256_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, -256),
		z0 = svdup_s64_x (p0, -256))

/*
** dup_m257_s64_x:
**	mov	z0\.d, #-257
**	ret
*/
TEST_UNIFORM_Z (dup_m257_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, -257),
		z0 = svdup_s64_x (p0, -257))

/*
** dup_m258_s64_x:
**	mov	(x[0-9]+), -258
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m258_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, -258),
		z0 = svdup_s64_x (p0, -258))

/*
** dup_m259_s64_x:
**	mov	(x[0-9]+), -259
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m259_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, -259),
		z0 = svdup_s64_x (p0, -259))

/*
** dup_m512_s64_x:
**	mov	z0\.d, #-512
**	ret
*/
TEST_UNIFORM_Z (dup_m512_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, -512),
		z0 = svdup_s64_x (p0, -512))

/*
** dup_m7f00_s64_x:
**	mov	z0\.d, #-32512
**	ret
*/
TEST_UNIFORM_Z (dup_m7f00_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, -0x7f00),
		z0 = svdup_s64_x (p0, -0x7f00))

/*
** dup_m7f01_s64_x:
**	mov	z0\.d, #-32513
**	ret
*/
TEST_UNIFORM_Z (dup_m7f01_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, -0x7f01),
		z0 = svdup_s64_x (p0, -0x7f01))

/*
** dup_m7f02_s64_x:
**	mov	(x[0-9]+), -32514
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m7f02_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, -0x7f02),
		z0 = svdup_s64_x (p0, -0x7f02))

/*
** dup_m7ffe_s64_x:
**	mov	(x[0-9]+), -32766
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m7ffe_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, -0x7ffe),
		z0 = svdup_s64_x (p0, -0x7ffe))

/*
** dup_m7fff_s64_x:
**	mov	z0\.d, #-32767
**	ret
*/
TEST_UNIFORM_Z (dup_m7fff_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, -0x7fff),
		z0 = svdup_s64_x (p0, -0x7fff))

/*
** dup_m8000_s64_x:
**	mov	z0\.d, #-32768
**	ret
*/
TEST_UNIFORM_Z (dup_m8000_s64_x, svint64_t,
		z0 = svdup_n_s64_x (p0, -0x8000),
		z0 = svdup_s64_x (p0, -0x8000))

/*
** dup_x0_s64_x:
**	mov	z0\.d, x0
**	ret
*/
TEST_UNIFORM_ZX (dup_x0_s64_x, svint64_t, int64_t,
		z0 = svdup_n_s64_x (p0, x0),
		z0 = svdup_s64_x (p0, x0))
