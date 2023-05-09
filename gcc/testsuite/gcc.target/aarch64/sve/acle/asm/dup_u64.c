/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dup_1_u64:
**	mov	z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (dup_1_u64, svuint64_t,
		z0 = svdup_n_u64 (1),
		z0 = svdup_u64 (1))

/*
** dup_127_u64:
**	mov	z0\.d, #127
**	ret
*/
TEST_UNIFORM_Z (dup_127_u64, svuint64_t,
		z0 = svdup_n_u64 (127),
		z0 = svdup_u64 (127))

/*
** dup_128_u64:
**	mov	z0\.d, #128
**	ret
*/
TEST_UNIFORM_Z (dup_128_u64, svuint64_t,
		z0 = svdup_n_u64 (128),
		z0 = svdup_u64 (128))

/*
** dup_129_u64:
**	mov	(x[0-9]+), 129
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_129_u64, svuint64_t,
		z0 = svdup_n_u64 (129),
		z0 = svdup_u64 (129))

/*
** dup_253_u64:
**	mov	(x[0-9]+), 253
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_253_u64, svuint64_t,
		z0 = svdup_n_u64 (253),
		z0 = svdup_u64 (253))

/*
** dup_254_u64:
**	mov	z0\.d, #254
**	ret
*/
TEST_UNIFORM_Z (dup_254_u64, svuint64_t,
		z0 = svdup_n_u64 (254),
		z0 = svdup_u64 (254))

/*
** dup_255_u64:
**	mov	z0\.d, #255
**	ret
*/
TEST_UNIFORM_Z (dup_255_u64, svuint64_t,
		z0 = svdup_n_u64 (255),
		z0 = svdup_u64 (255))

/*
** dup_256_u64:
**	mov	z0\.d, #256
**	ret
*/
TEST_UNIFORM_Z (dup_256_u64, svuint64_t,
		z0 = svdup_n_u64 (256),
		z0 = svdup_u64 (256))

/*
** dup_257_u64:
**	mov	(x[0-9]+), 257
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_257_u64, svuint64_t,
		z0 = svdup_n_u64 (257),
		z0 = svdup_u64 (257))

/*
** dup_512_u64:
**	mov	z0\.d, #512
**	ret
*/
TEST_UNIFORM_Z (dup_512_u64, svuint64_t,
		z0 = svdup_n_u64 (512),
		z0 = svdup_u64 (512))

/*
** dup_7f00_u64:
**	mov	z0\.d, #32512
**	ret
*/
TEST_UNIFORM_Z (dup_7f00_u64, svuint64_t,
		z0 = svdup_n_u64 (0x7f00),
		z0 = svdup_u64 (0x7f00))

/*
** dup_7f01_u64:
**	mov	(x[0-9]+), 32513
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_7f01_u64, svuint64_t,
		z0 = svdup_n_u64 (0x7f01),
		z0 = svdup_u64 (0x7f01))

/*
** dup_7ffd_u64:
**	mov	(x[0-9]+), 32765
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_7ffd_u64, svuint64_t,
		z0 = svdup_n_u64 (0x7ffd),
		z0 = svdup_u64 (0x7ffd))

/*
** dup_7ffe_u64:
**	mov	z0\.d, #32766
**	ret
*/
TEST_UNIFORM_Z (dup_7ffe_u64, svuint64_t,
		z0 = svdup_n_u64 (0x7ffe),
		z0 = svdup_u64 (0x7ffe))

/*
** dup_7fff_u64:
**	mov	z0\.d, #32767
**	ret
*/
TEST_UNIFORM_Z (dup_7fff_u64, svuint64_t,
		z0 = svdup_n_u64 (0x7fff),
		z0 = svdup_u64 (0x7fff))

/*
** dup_m1_u64:
**	mov	z0\.b, #-1
**	ret
*/
TEST_UNIFORM_Z (dup_m1_u64, svuint64_t,
		z0 = svdup_n_u64 (-1),
		z0 = svdup_u64 (-1))

/*
** dup_m128_u64:
**	mov	z0\.d, #-128
**	ret
*/
TEST_UNIFORM_Z (dup_m128_u64, svuint64_t,
		z0 = svdup_n_u64 (-128),
		z0 = svdup_u64 (-128))

/*
** dup_m129_u64:
**	mov	z0\.d, #-129
**	ret
*/
TEST_UNIFORM_Z (dup_m129_u64, svuint64_t,
		z0 = svdup_n_u64 (-129),
		z0 = svdup_u64 (-129))

/*
** dup_m130_u64:
**	mov	(x[0-9]+), -130
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m130_u64, svuint64_t,
		z0 = svdup_n_u64 (-130),
		z0 = svdup_u64 (-130))

/*
** dup_m254_u64:
**	mov	(x[0-9]+), -254
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m254_u64, svuint64_t,
		z0 = svdup_n_u64 (-254),
		z0 = svdup_u64 (-254))

/*
** dup_m255_u64:
**	mov	z0\.d, #-255
**	ret
*/
TEST_UNIFORM_Z (dup_m255_u64, svuint64_t,
		z0 = svdup_n_u64 (-255),
		z0 = svdup_u64 (-255))

/*
** dup_m256_u64:
**	mov	z0\.d, #-256
**	ret
*/
TEST_UNIFORM_Z (dup_m256_u64, svuint64_t,
		z0 = svdup_n_u64 (-256),
		z0 = svdup_u64 (-256))

/*
** dup_m257_u64:
**	mov	z0\.d, #-257
**	ret
*/
TEST_UNIFORM_Z (dup_m257_u64, svuint64_t,
		z0 = svdup_n_u64 (-257),
		z0 = svdup_u64 (-257))

/*
** dup_m258_u64:
**	mov	(x[0-9]+), -258
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m258_u64, svuint64_t,
		z0 = svdup_n_u64 (-258),
		z0 = svdup_u64 (-258))

/*
** dup_m259_u64:
**	mov	(x[0-9]+), -259
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m259_u64, svuint64_t,
		z0 = svdup_n_u64 (-259),
		z0 = svdup_u64 (-259))

/*
** dup_m512_u64:
**	mov	z0\.d, #-512
**	ret
*/
TEST_UNIFORM_Z (dup_m512_u64, svuint64_t,
		z0 = svdup_n_u64 (-512),
		z0 = svdup_u64 (-512))

/*
** dup_m7f00_u64:
**	mov	z0\.d, #-32512
**	ret
*/
TEST_UNIFORM_Z (dup_m7f00_u64, svuint64_t,
		z0 = svdup_n_u64 (-0x7f00),
		z0 = svdup_u64 (-0x7f00))

/*
** dup_m7f01_u64:
**	mov	z0\.d, #-32513
**	ret
*/
TEST_UNIFORM_Z (dup_m7f01_u64, svuint64_t,
		z0 = svdup_n_u64 (-0x7f01),
		z0 = svdup_u64 (-0x7f01))

/*
** dup_m7f02_u64:
**	mov	(x[0-9]+), -32514
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m7f02_u64, svuint64_t,
		z0 = svdup_n_u64 (-0x7f02),
		z0 = svdup_u64 (-0x7f02))

/*
** dup_m7ffe_u64:
**	mov	(x[0-9]+), -32766
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m7ffe_u64, svuint64_t,
		z0 = svdup_n_u64 (-0x7ffe),
		z0 = svdup_u64 (-0x7ffe))

/*
** dup_m7fff_u64:
**	mov	z0\.d, #-32767
**	ret
*/
TEST_UNIFORM_Z (dup_m7fff_u64, svuint64_t,
		z0 = svdup_n_u64 (-0x7fff),
		z0 = svdup_u64 (-0x7fff))

/*
** dup_m8000_u64:
**	mov	z0\.d, #-32768
**	ret
*/
TEST_UNIFORM_Z (dup_m8000_u64, svuint64_t,
		z0 = svdup_n_u64 (-0x8000),
		z0 = svdup_u64 (-0x8000))

/*
** dup_x0_u64:
**	mov	z0\.d, x0
**	ret
*/
TEST_UNIFORM_ZX (dup_x0_u64, svuint64_t, uint64_t,
		 z0 = svdup_n_u64 (x0),
		 z0 = svdup_u64 (x0))

/*
** dup_1_u64_m:
**	mov	z0\.d, p0/m, #1
**	ret
*/
TEST_UNIFORM_Z (dup_1_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, 1),
		z0 = svdup_u64_m (z0, p0, 1))

/*
** dup_127_u64_m:
**	mov	z0\.d, p0/m, #127
**	ret
*/
TEST_UNIFORM_Z (dup_127_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, 127),
		z0 = svdup_u64_m (z0, p0, 127))

/*
** dup_128_u64_m:
**	mov	(z[0-9]+\.d), #128
**	sel	z0\.d, p0, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (dup_128_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, 128),
		z0 = svdup_u64_m (z0, p0, 128))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_129_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, 129),
		z0 = svdup_u64_m (z0, p0, 129))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_253_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, 253),
		z0 = svdup_u64_m (z0, p0, 253))

/*
** dup_254_u64_m:
**	mov	(z[0-9]+\.d), #254
**	sel	z0\.d, p0, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (dup_254_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, 254),
		z0 = svdup_u64_m (z0, p0, 254))

/*
** dup_255_u64_m:
**	mov	(z[0-9]+\.d), #255
**	sel	z0\.d, p0, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (dup_255_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, 255),
		z0 = svdup_u64_m (z0, p0, 255))

/*
** dup_256_u64_m:
**	mov	z0\.d, p0/m, #256
**	ret
*/
TEST_UNIFORM_Z (dup_256_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, 256),
		z0 = svdup_u64_m (z0, p0, 256))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_257_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, 257),
		z0 = svdup_u64_m (z0, p0, 257))

/*
** dup_512_u64_m:
**	mov	z0\.d, p0/m, #512
**	ret
*/
TEST_UNIFORM_Z (dup_512_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, 512),
		z0 = svdup_u64_m (z0, p0, 512))

/*
** dup_7f00_u64_m:
**	mov	z0\.d, p0/m, #32512
**	ret
*/
TEST_UNIFORM_Z (dup_7f00_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, 0x7f00),
		z0 = svdup_u64_m (z0, p0, 0x7f00))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_7f01_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, 0x7f01),
		z0 = svdup_u64_m (z0, p0, 0x7f01))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_7ffd_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, 0x7ffd),
		z0 = svdup_u64_m (z0, p0, 0x7ffd))

/*
** dup_7ffe_u64_m:
**	mov	(z[0-9]+\.d), #32766
**	sel	z0\.d, p0, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (dup_7ffe_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, 0x7ffe),
		z0 = svdup_u64_m (z0, p0, 0x7ffe))

/*
** dup_7fff_u64_m:
**	mov	(z[0-9]+\.d), #32767
**	sel	z0\.d, p0, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (dup_7fff_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, 0x7fff),
		z0 = svdup_u64_m (z0, p0, 0x7fff))

/*
** dup_m1_u64_m:
**	mov	z0\.d, p0/m, #-1
**	ret
*/
TEST_UNIFORM_Z (dup_m1_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, -1),
		z0 = svdup_u64_m (z0, p0, -1))

/*
** dup_m128_u64_m:
**	mov	z0\.d, p0/m, #-128
**	ret
*/
TEST_UNIFORM_Z (dup_m128_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, -128),
		z0 = svdup_u64_m (z0, p0, -128))

/*
** dup_m129_u64_m:
**	mov	(z[0-9]+\.d), #-129
**	sel	z0\.d, p0, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (dup_m129_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, -129),
		z0 = svdup_u64_m (z0, p0, -129))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m130_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, -130),
		z0 = svdup_u64_m (z0, p0, -130))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m254_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, -254),
		z0 = svdup_u64_m (z0, p0, -254))

/*
** dup_m255_u64_m:
**	mov	(z[0-9]+\.d), #-255
**	sel	z0\.d, p0, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (dup_m255_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, -255),
		z0 = svdup_u64_m (z0, p0, -255))

/*
** dup_m256_u64_m:
**	mov	z0\.d, p0/m, #-256
**	ret
*/
TEST_UNIFORM_Z (dup_m256_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, -256),
		z0 = svdup_u64_m (z0, p0, -256))

/*
** dup_m257_u64_m:
**	mov	(z[0-9]+\.d), #-257
**	sel	z0\.d, p0, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (dup_m257_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, -257),
		z0 = svdup_u64_m (z0, p0, -257))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m258_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, -258),
		z0 = svdup_u64_m (z0, p0, -258))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m259_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, -259),
		z0 = svdup_u64_m (z0, p0, -259))

/*
** dup_m512_u64_m:
**	mov	z0\.d, p0/m, #-512
**	ret
*/
TEST_UNIFORM_Z (dup_m512_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, -512),
		z0 = svdup_u64_m (z0, p0, -512))

/*
** dup_m7f00_u64_m:
**	mov	z0\.d, p0/m, #-32512
**	ret
*/
TEST_UNIFORM_Z (dup_m7f00_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, -0x7f00),
		z0 = svdup_u64_m (z0, p0, -0x7f00))

/*
** dup_m7f01_u64_m:
**	mov	(z[0-9]+\.d), #-32513
**	sel	z0\.d, p0, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (dup_m7f01_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, -0x7f01),
		z0 = svdup_u64_m (z0, p0, -0x7f01))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m7f02_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, -0x7f02),
		z0 = svdup_u64_m (z0, p0, -0x7f02))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m7ffe_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, -0x7ffe),
		z0 = svdup_u64_m (z0, p0, -0x7ffe))

/*
** dup_m7fff_u64_m:
**	mov	(z[0-9]+\.d), #-32767
**	sel	z0\.d, p0, \1, z0\.d
**	ret
*/
TEST_UNIFORM_Z (dup_m7fff_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, -0x7fff),
		z0 = svdup_u64_m (z0, p0, -0x7fff))

/*
** dup_m8000_u64_m:
**	mov	z0\.d, p0/m, #-32768
**	ret
*/
TEST_UNIFORM_Z (dup_m8000_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, -0x8000),
		z0 = svdup_u64_m (z0, p0, -0x8000))

/*
** dup_0_u64_m:
**	mov	z0\.d, p0/m, #0
**	ret
*/
TEST_UNIFORM_Z (dup_0_u64_m, svuint64_t,
		z0 = svdup_n_u64_m (z0, p0, 0),
		z0 = svdup_u64_m (z0, p0, 0))

/*
** dup_x0_u64_m:
**	movprfx	z0, z1
**	mov	z0\.d, p0/m, x0
**	ret
*/
TEST_UNIFORM_ZX (dup_x0_u64_m, svuint64_t, uint64_t,
		z0 = svdup_n_u64_m (z1, p0, x0),
		z0 = svdup_u64_m (z1, p0, x0))

/*
** dup_1_u64_z:
**	mov	z0\.d, p0/z, #1
**	ret
*/
TEST_UNIFORM_Z (dup_1_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, 1),
		z0 = svdup_u64_z (p0, 1))

/*
** dup_127_u64_z:
**	mov	z0\.d, p0/z, #127
**	ret
*/
TEST_UNIFORM_Z (dup_127_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, 127),
		z0 = svdup_u64_z (p0, 127))

/*
** dup_128_u64_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.d), #128
**	sel	z0\.d, p0, \2, \1\.d
** |
**	mov	(z[0-9]+\.d), #128
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.d, p0, \3, \4\.d
** )
**	ret
*/
TEST_UNIFORM_Z (dup_128_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, 128),
		z0 = svdup_u64_z (p0, 128))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_129_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, 129),
		z0 = svdup_u64_z (p0, 129))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_253_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, 253),
		z0 = svdup_u64_z (p0, 253))

/*
** dup_254_u64_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.d), #254
**	sel	z0\.d, p0, \2, \1\.d
** |
**	mov	(z[0-9]+\.d), #254
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.d, p0, \3, \4\.d
** )
**	ret
*/
TEST_UNIFORM_Z (dup_254_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, 254),
		z0 = svdup_u64_z (p0, 254))

/*
** dup_255_u64_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.d), #255
**	sel	z0\.d, p0, \2, \1\.d
** |
**	mov	(z[0-9]+\.d), #255
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.d, p0, \3, \4\.d
** )
**	ret
*/
TEST_UNIFORM_Z (dup_255_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, 255),
		z0 = svdup_u64_z (p0, 255))

/*
** dup_256_u64_z:
**	mov	z0\.d, p0/z, #256
**	ret
*/
TEST_UNIFORM_Z (dup_256_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, 256),
		z0 = svdup_u64_z (p0, 256))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_257_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, 257),
		z0 = svdup_u64_z (p0, 257))

/*
** dup_512_u64_z:
**	mov	z0\.d, p0/z, #512
**	ret
*/
TEST_UNIFORM_Z (dup_512_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, 512),
		z0 = svdup_u64_z (p0, 512))

/*
** dup_7f00_u64_z:
**	mov	z0\.d, p0/z, #32512
**	ret
*/
TEST_UNIFORM_Z (dup_7f00_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, 0x7f00),
		z0 = svdup_u64_z (p0, 0x7f00))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_7f01_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, 0x7f01),
		z0 = svdup_u64_z (p0, 0x7f01))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_7ffd_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, 0x7ffd),
		z0 = svdup_u64_z (p0, 0x7ffd))

/*
** dup_7ffe_u64_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.d), #32766
**	sel	z0\.d, p0, \2, \1\.d
** |
**	mov	(z[0-9]+\.d), #32766
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.d, p0, \3, \4\.d
** )
**	ret
*/
TEST_UNIFORM_Z (dup_7ffe_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, 0x7ffe),
		z0 = svdup_u64_z (p0, 0x7ffe))

/*
** dup_7fff_u64_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.d), #32767
**	sel	z0\.d, p0, \2, \1\.d
** |
**	mov	(z[0-9]+\.d), #32767
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.d, p0, \3, \4\.d
** )
**	ret
*/
TEST_UNIFORM_Z (dup_7fff_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, 0x7fff),
		z0 = svdup_u64_z (p0, 0x7fff))

/*
** dup_m1_u64_z:
**	mov	z0\.d, p0/z, #-1
**	ret
*/
TEST_UNIFORM_Z (dup_m1_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, -1),
		z0 = svdup_u64_z (p0, -1))

/*
** dup_m128_u64_z:
**	mov	z0\.d, p0/z, #-128
**	ret
*/
TEST_UNIFORM_Z (dup_m128_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, -128),
		z0 = svdup_u64_z (p0, -128))

/*
** dup_m129_u64_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.d), #-129
**	sel	z0\.d, p0, \2, \1\.d
** |
**	mov	(z[0-9]+\.d), #-129
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.d, p0, \3, \4\.d
** )
**	ret
*/
TEST_UNIFORM_Z (dup_m129_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, -129),
		z0 = svdup_u64_z (p0, -129))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m130_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, -130),
		z0 = svdup_u64_z (p0, -130))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m254_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, -254),
		z0 = svdup_u64_z (p0, -254))

/*
** dup_m255_u64_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.d), #-255
**	sel	z0\.d, p0, \2, \1\.d
** |
**	mov	(z[0-9]+\.d), #-255
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.d, p0, \3, \4\.d
** )
**	ret
*/
TEST_UNIFORM_Z (dup_m255_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, -255),
		z0 = svdup_u64_z (p0, -255))

/*
** dup_m256_u64_z:
**	mov	z0\.d, p0/z, #-256
**	ret
*/
TEST_UNIFORM_Z (dup_m256_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, -256),
		z0 = svdup_u64_z (p0, -256))

/*
** dup_m257_u64_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.d), #-257
**	sel	z0\.d, p0, \2, \1\.d
** |
**	mov	(z[0-9]+\.d), #-257
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.d, p0, \3, \4\.d
** )
**	ret
*/
TEST_UNIFORM_Z (dup_m257_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, -257),
		z0 = svdup_u64_z (p0, -257))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m258_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, -258),
		z0 = svdup_u64_z (p0, -258))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m259_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, -259),
		z0 = svdup_u64_z (p0, -259))

/*
** dup_m512_u64_z:
**	mov	z0\.d, p0/z, #-512
**	ret
*/
TEST_UNIFORM_Z (dup_m512_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, -512),
		z0 = svdup_u64_z (p0, -512))

/*
** dup_m7f00_u64_z:
**	mov	z0\.d, p0/z, #-32512
**	ret
*/
TEST_UNIFORM_Z (dup_m7f00_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, -0x7f00),
		z0 = svdup_u64_z (p0, -0x7f00))

/*
** dup_m7f01_u64_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.d), #-32513
**	sel	z0\.d, p0, \2, \1\.d
** |
**	mov	(z[0-9]+\.d), #-32513
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.d, p0, \3, \4\.d
** )
**	ret
*/
TEST_UNIFORM_Z (dup_m7f01_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, -0x7f01),
		z0 = svdup_u64_z (p0, -0x7f01))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m7f02_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, -0x7f02),
		z0 = svdup_u64_z (p0, -0x7f02))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m7ffe_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, -0x7ffe),
		z0 = svdup_u64_z (p0, -0x7ffe))

/*
** dup_m7fff_u64_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.d), #-32767
**	sel	z0\.d, p0, \2, \1\.d
** |
**	mov	(z[0-9]+\.d), #-32767
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.d, p0, \3, \4\.d
** )
**	ret
*/
TEST_UNIFORM_Z (dup_m7fff_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, -0x7fff),
		z0 = svdup_u64_z (p0, -0x7fff))

/*
** dup_m8000_u64_z:
**	mov	z0\.d, p0/z, #-32768
**	ret
*/
TEST_UNIFORM_Z (dup_m8000_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, -0x8000),
		z0 = svdup_u64_z (p0, -0x8000))

/*
** dup_0_u64_z:
**	mov	z0\.[bhsd], #0
**	ret
*/
TEST_UNIFORM_Z (dup_0_u64_z, svuint64_t,
		z0 = svdup_n_u64_z (p0, 0),
		z0 = svdup_u64_z (p0, 0))

/*
** dup_x0_u64_z:
**	movprfx	z0\.d, p0/z, z0\.d
**	mov	z0\.d, p0/m, x0
**	ret
*/
TEST_UNIFORM_ZX (dup_x0_u64_z, svuint64_t, uint64_t,
		z0 = svdup_n_u64_z (p0, x0),
		z0 = svdup_u64_z (p0, x0))

/*
** dup_1_u64_x:
**	mov	z0\.d, #1
**	ret
*/
TEST_UNIFORM_Z (dup_1_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, 1),
		z0 = svdup_u64_x (p0, 1))

/*
** dup_127_u64_x:
**	mov	z0\.d, #127
**	ret
*/
TEST_UNIFORM_Z (dup_127_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, 127),
		z0 = svdup_u64_x (p0, 127))

/*
** dup_128_u64_x:
**	mov	z0\.d, #128
**	ret
*/
TEST_UNIFORM_Z (dup_128_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, 128),
		z0 = svdup_u64_x (p0, 128))

/*
** dup_129_u64_x:
**	mov	(x[0-9]+), 129
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_129_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, 129),
		z0 = svdup_u64_x (p0, 129))

/*
** dup_253_u64_x:
**	mov	(x[0-9]+), 253
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_253_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, 253),
		z0 = svdup_u64_x (p0, 253))

/*
** dup_254_u64_x:
**	mov	z0\.d, #254
**	ret
*/
TEST_UNIFORM_Z (dup_254_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, 254),
		z0 = svdup_u64_x (p0, 254))

/*
** dup_255_u64_x:
**	mov	z0\.d, #255
**	ret
*/
TEST_UNIFORM_Z (dup_255_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, 255),
		z0 = svdup_u64_x (p0, 255))

/*
** dup_256_u64_x:
**	mov	z0\.d, #256
**	ret
*/
TEST_UNIFORM_Z (dup_256_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, 256),
		z0 = svdup_u64_x (p0, 256))

/*
** dup_257_u64_x:
**	mov	(x[0-9]+), 257
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_257_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, 257),
		z0 = svdup_u64_x (p0, 257))

/*
** dup_512_u64_x:
**	mov	z0\.d, #512
**	ret
*/
TEST_UNIFORM_Z (dup_512_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, 512),
		z0 = svdup_u64_x (p0, 512))

/*
** dup_7f00_u64_x:
**	mov	z0\.d, #32512
**	ret
*/
TEST_UNIFORM_Z (dup_7f00_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, 0x7f00),
		z0 = svdup_u64_x (p0, 0x7f00))

/*
** dup_7f01_u64_x:
**	mov	(x[0-9]+), 32513
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_7f01_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, 0x7f01),
		z0 = svdup_u64_x (p0, 0x7f01))

/*
** dup_7ffd_u64_x:
**	mov	(x[0-9]+), 32765
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_7ffd_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, 0x7ffd),
		z0 = svdup_u64_x (p0, 0x7ffd))

/*
** dup_7ffe_u64_x:
**	mov	z0\.d, #32766
**	ret
*/
TEST_UNIFORM_Z (dup_7ffe_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, 0x7ffe),
		z0 = svdup_u64_x (p0, 0x7ffe))

/*
** dup_7fff_u64_x:
**	mov	z0\.d, #32767
**	ret
*/
TEST_UNIFORM_Z (dup_7fff_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, 0x7fff),
		z0 = svdup_u64_x (p0, 0x7fff))

/*
** dup_m1_u64_x:
**	mov	z0\.b, #-1
**	ret
*/
TEST_UNIFORM_Z (dup_m1_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, -1),
		z0 = svdup_u64_x (p0, -1))

/*
** dup_m128_u64_x:
**	mov	z0\.d, #-128
**	ret
*/
TEST_UNIFORM_Z (dup_m128_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, -128),
		z0 = svdup_u64_x (p0, -128))

/*
** dup_m129_u64_x:
**	mov	z0\.d, #-129
**	ret
*/
TEST_UNIFORM_Z (dup_m129_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, -129),
		z0 = svdup_u64_x (p0, -129))

/*
** dup_m130_u64_x:
**	mov	(x[0-9]+), -130
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m130_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, -130),
		z0 = svdup_u64_x (p0, -130))

/*
** dup_m254_u64_x:
**	mov	(x[0-9]+), -254
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m254_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, -254),
		z0 = svdup_u64_x (p0, -254))

/*
** dup_m255_u64_x:
**	mov	z0\.d, #-255
**	ret
*/
TEST_UNIFORM_Z (dup_m255_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, -255),
		z0 = svdup_u64_x (p0, -255))

/*
** dup_m256_u64_x:
**	mov	z0\.d, #-256
**	ret
*/
TEST_UNIFORM_Z (dup_m256_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, -256),
		z0 = svdup_u64_x (p0, -256))

/*
** dup_m257_u64_x:
**	mov	z0\.d, #-257
**	ret
*/
TEST_UNIFORM_Z (dup_m257_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, -257),
		z0 = svdup_u64_x (p0, -257))

/*
** dup_m258_u64_x:
**	mov	(x[0-9]+), -258
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m258_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, -258),
		z0 = svdup_u64_x (p0, -258))

/*
** dup_m259_u64_x:
**	mov	(x[0-9]+), -259
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m259_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, -259),
		z0 = svdup_u64_x (p0, -259))

/*
** dup_m512_u64_x:
**	mov	z0\.d, #-512
**	ret
*/
TEST_UNIFORM_Z (dup_m512_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, -512),
		z0 = svdup_u64_x (p0, -512))

/*
** dup_m7f00_u64_x:
**	mov	z0\.d, #-32512
**	ret
*/
TEST_UNIFORM_Z (dup_m7f00_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, -0x7f00),
		z0 = svdup_u64_x (p0, -0x7f00))

/*
** dup_m7f01_u64_x:
**	mov	z0\.d, #-32513
**	ret
*/
TEST_UNIFORM_Z (dup_m7f01_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, -0x7f01),
		z0 = svdup_u64_x (p0, -0x7f01))

/*
** dup_m7f02_u64_x:
**	mov	(x[0-9]+), -32514
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m7f02_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, -0x7f02),
		z0 = svdup_u64_x (p0, -0x7f02))

/*
** dup_m7ffe_u64_x:
**	mov	(x[0-9]+), -32766
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m7ffe_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, -0x7ffe),
		z0 = svdup_u64_x (p0, -0x7ffe))

/*
** dup_m7fff_u64_x:
**	mov	z0\.d, #-32767
**	ret
*/
TEST_UNIFORM_Z (dup_m7fff_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, -0x7fff),
		z0 = svdup_u64_x (p0, -0x7fff))

/*
** dup_m8000_u64_x:
**	mov	z0\.d, #-32768
**	ret
*/
TEST_UNIFORM_Z (dup_m8000_u64_x, svuint64_t,
		z0 = svdup_n_u64_x (p0, -0x8000),
		z0 = svdup_u64_x (p0, -0x8000))

/*
** dup_x0_u64_x:
**	mov	z0\.d, x0
**	ret
*/
TEST_UNIFORM_ZX (dup_x0_u64_x, svuint64_t, uint64_t,
		z0 = svdup_n_u64_x (p0, x0),
		z0 = svdup_u64_x (p0, x0))
