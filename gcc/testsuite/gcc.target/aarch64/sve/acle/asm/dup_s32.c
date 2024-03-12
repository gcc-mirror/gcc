/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dup_1_s32:
**	mov	z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (dup_1_s32, svint32_t,
		z0 = svdup_n_s32 (1),
		z0 = svdup_s32 (1))

/*
** dup_127_s32:
**	mov	z0\.s, #127
**	ret
*/
TEST_UNIFORM_Z (dup_127_s32, svint32_t,
		z0 = svdup_n_s32 (127),
		z0 = svdup_s32 (127))

/*
** dup_128_s32:
**	mov	z0\.s, #128
**	ret
*/
TEST_UNIFORM_Z (dup_128_s32, svint32_t,
		z0 = svdup_n_s32 (128),
		z0 = svdup_s32 (128))

/*
** dup_129_s32:
**	movi	v([0-9]+)\.4s, 0x81
**	dup	z0\.q, z\1\.q\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_129_s32, svint32_t,
		z0 = svdup_n_s32 (129),
		z0 = svdup_s32 (129))

/*
** dup_253_s32:
**	movi	v([0-9]+)\.4s, 0xfd
**	dup	z0\.q, z\1\.q\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_253_s32, svint32_t,
		z0 = svdup_n_s32 (253),
		z0 = svdup_s32 (253))

/*
** dup_254_s32:
**	mov	z0\.s, #254
**	ret
*/
TEST_UNIFORM_Z (dup_254_s32, svint32_t,
		z0 = svdup_n_s32 (254),
		z0 = svdup_s32 (254))

/*
** dup_255_s32:
**	mov	z0\.s, #255
**	ret
*/
TEST_UNIFORM_Z (dup_255_s32, svint32_t,
		z0 = svdup_n_s32 (255),
		z0 = svdup_s32 (255))

/*
** dup_256_s32:
**	mov	z0\.s, #256
**	ret
*/
TEST_UNIFORM_Z (dup_256_s32, svint32_t,
		z0 = svdup_n_s32 (256),
		z0 = svdup_s32 (256))

/*
** dup_257_s32:
**	mov	(w[0-9]+), 257
**	mov	z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (dup_257_s32, svint32_t,
		z0 = svdup_n_s32 (257),
		z0 = svdup_s32 (257))

/*
** dup_512_s32:
**	mov	z0\.s, #512
**	ret
*/
TEST_UNIFORM_Z (dup_512_s32, svint32_t,
		z0 = svdup_n_s32 (512),
		z0 = svdup_s32 (512))

/*
** dup_7f00_s32:
**	mov	z0\.s, #32512
**	ret
*/
TEST_UNIFORM_Z (dup_7f00_s32, svint32_t,
		z0 = svdup_n_s32 (0x7f00),
		z0 = svdup_s32 (0x7f00))

/*
** dup_7f01_s32:
**	mov	(w[0-9]+), 32513
**	mov	z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (dup_7f01_s32, svint32_t,
		z0 = svdup_n_s32 (0x7f01),
		z0 = svdup_s32 (0x7f01))

/*
** dup_7ffd_s32:
**	mov	(w[0-9]+), 32765
**	mov	z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (dup_7ffd_s32, svint32_t,
		z0 = svdup_n_s32 (0x7ffd),
		z0 = svdup_s32 (0x7ffd))

/*
** dup_7ffe_s32:
**	mov	z0\.s, #32766
**	ret
*/
TEST_UNIFORM_Z (dup_7ffe_s32, svint32_t,
		z0 = svdup_n_s32 (0x7ffe),
		z0 = svdup_s32 (0x7ffe))

/*
** dup_7fff_s32:
**	mov	z0\.s, #32767
**	ret
*/
TEST_UNIFORM_Z (dup_7fff_s32, svint32_t,
		z0 = svdup_n_s32 (0x7fff),
		z0 = svdup_s32 (0x7fff))

/*
** dup_m1_s32:
**	mov	z0\.b, #-1
**	ret
*/
TEST_UNIFORM_Z (dup_m1_s32, svint32_t,
		z0 = svdup_n_s32 (-1),
		z0 = svdup_s32 (-1))

/*
** dup_m128_s32:
**	mov	z0\.s, #-128
**	ret
*/
TEST_UNIFORM_Z (dup_m128_s32, svint32_t,
		z0 = svdup_n_s32 (-128),
		z0 = svdup_s32 (-128))

/*
** dup_m129_s32:
**	mov	z0\.s, #-129
**	ret
*/
TEST_UNIFORM_Z (dup_m129_s32, svint32_t,
		z0 = svdup_n_s32 (-129),
		z0 = svdup_s32 (-129))

/*
** dup_m130_s32:
**	mvni	v([0-9]+)\.4s, 0x81
**	dup	z0\.q, z\1\.q\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_m130_s32, svint32_t,
		z0 = svdup_n_s32 (-130),
		z0 = svdup_s32 (-130))

/*
** dup_m254_s32:
**	mvni	v([0-9]+)\.4s, 0xfd
**	dup	z0\.q, z\1\.q\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_m254_s32, svint32_t,
		z0 = svdup_n_s32 (-254),
		z0 = svdup_s32 (-254))

/*
** dup_m255_s32:
**	mov	z0\.s, #-255
**	ret
*/
TEST_UNIFORM_Z (dup_m255_s32, svint32_t,
		z0 = svdup_n_s32 (-255),
		z0 = svdup_s32 (-255))

/*
** dup_m256_s32:
**	mov	z0\.s, #-256
**	ret
*/
TEST_UNIFORM_Z (dup_m256_s32, svint32_t,
		z0 = svdup_n_s32 (-256),
		z0 = svdup_s32 (-256))

/*
** dup_m257_s32:
**	mov	z0\.s, #-257
**	ret
*/
TEST_UNIFORM_Z (dup_m257_s32, svint32_t,
		z0 = svdup_n_s32 (-257),
		z0 = svdup_s32 (-257))

/*
** dup_m258_s32:
**	mov	(w[0-9]+), -258
**	mov	z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m258_s32, svint32_t,
		z0 = svdup_n_s32 (-258),
		z0 = svdup_s32 (-258))

/*
** dup_m259_s32:
**	mov	(w[0-9]+), -259
**	mov	z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m259_s32, svint32_t,
		z0 = svdup_n_s32 (-259),
		z0 = svdup_s32 (-259))

/*
** dup_m512_s32:
**	mov	z0\.s, #-512
**	ret
*/
TEST_UNIFORM_Z (dup_m512_s32, svint32_t,
		z0 = svdup_n_s32 (-512),
		z0 = svdup_s32 (-512))

/*
** dup_m7f00_s32:
**	mov	z0\.s, #-32512
**	ret
*/
TEST_UNIFORM_Z (dup_m7f00_s32, svint32_t,
		z0 = svdup_n_s32 (-0x7f00),
		z0 = svdup_s32 (-0x7f00))

/*
** dup_m7f01_s32:
**	mov	z0\.s, #-32513
**	ret
*/
TEST_UNIFORM_Z (dup_m7f01_s32, svint32_t,
		z0 = svdup_n_s32 (-0x7f01),
		z0 = svdup_s32 (-0x7f01))

/*
** dup_m7f02_s32:
**	mov	(w[0-9]+), -32514
**	mov	z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m7f02_s32, svint32_t,
		z0 = svdup_n_s32 (-0x7f02),
		z0 = svdup_s32 (-0x7f02))

/*
** dup_m7ffe_s32:
**	mov	(w[0-9]+), -32766
**	mov	z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m7ffe_s32, svint32_t,
		z0 = svdup_n_s32 (-0x7ffe),
		z0 = svdup_s32 (-0x7ffe))

/*
** dup_m7fff_s32:
**	mov	z0\.s, #-32767
**	ret
*/
TEST_UNIFORM_Z (dup_m7fff_s32, svint32_t,
		z0 = svdup_n_s32 (-0x7fff),
		z0 = svdup_s32 (-0x7fff))

/*
** dup_m8000_s32:
**	mov	z0\.s, #-32768
**	ret
*/
TEST_UNIFORM_Z (dup_m8000_s32, svint32_t,
		z0 = svdup_n_s32 (-0x8000),
		z0 = svdup_s32 (-0x8000))

/*
** dup_w0_s32:
**	mov	z0\.s, w0
**	ret
*/
TEST_UNIFORM_ZX (dup_w0_s32, svint32_t, int32_t,
		 z0 = svdup_n_s32 (x0),
		 z0 = svdup_s32 (x0))

/*
** dup_1_s32_m:
**	mov	z0\.s, p0/m, #1
**	ret
*/
TEST_UNIFORM_Z (dup_1_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, 1),
		z0 = svdup_s32_m (z0, p0, 1))

/*
** dup_127_s32_m:
**	mov	z0\.s, p0/m, #127
**	ret
*/
TEST_UNIFORM_Z (dup_127_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, 127),
		z0 = svdup_s32_m (z0, p0, 127))

/*
** dup_128_s32_m:
**	mov	(z[0-9]+\.s), #128
**	sel	z0\.s, p0, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (dup_128_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, 128),
		z0 = svdup_s32_m (z0, p0, 128))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_129_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, 129),
		z0 = svdup_s32_m (z0, p0, 129))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_253_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, 253),
		z0 = svdup_s32_m (z0, p0, 253))

/*
** dup_254_s32_m:
**	mov	(z[0-9]+\.s), #254
**	sel	z0\.s, p0, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (dup_254_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, 254),
		z0 = svdup_s32_m (z0, p0, 254))

/*
** dup_255_s32_m:
**	mov	(z[0-9]+\.s), #255
**	sel	z0\.s, p0, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (dup_255_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, 255),
		z0 = svdup_s32_m (z0, p0, 255))

/*
** dup_256_s32_m:
**	mov	z0\.s, p0/m, #256
**	ret
*/
TEST_UNIFORM_Z (dup_256_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, 256),
		z0 = svdup_s32_m (z0, p0, 256))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_257_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, 257),
		z0 = svdup_s32_m (z0, p0, 257))

/*
** dup_512_s32_m:
**	mov	z0\.s, p0/m, #512
**	ret
*/
TEST_UNIFORM_Z (dup_512_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, 512),
		z0 = svdup_s32_m (z0, p0, 512))

/*
** dup_7f00_s32_m:
**	mov	z0\.s, p0/m, #32512
**	ret
*/
TEST_UNIFORM_Z (dup_7f00_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, 0x7f00),
		z0 = svdup_s32_m (z0, p0, 0x7f00))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_7f01_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, 0x7f01),
		z0 = svdup_s32_m (z0, p0, 0x7f01))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_7ffd_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, 0x7ffd),
		z0 = svdup_s32_m (z0, p0, 0x7ffd))

/*
** dup_7ffe_s32_m:
**	mov	(z[0-9]+\.s), #32766
**	sel	z0\.s, p0, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (dup_7ffe_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, 0x7ffe),
		z0 = svdup_s32_m (z0, p0, 0x7ffe))

/*
** dup_7fff_s32_m:
**	mov	(z[0-9]+\.s), #32767
**	sel	z0\.s, p0, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (dup_7fff_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, 0x7fff),
		z0 = svdup_s32_m (z0, p0, 0x7fff))

/*
** dup_m1_s32_m:
**	mov	z0\.s, p0/m, #-1
**	ret
*/
TEST_UNIFORM_Z (dup_m1_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, -1),
		z0 = svdup_s32_m (z0, p0, -1))

/*
** dup_m128_s32_m:
**	mov	z0\.s, p0/m, #-128
**	ret
*/
TEST_UNIFORM_Z (dup_m128_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, -128),
		z0 = svdup_s32_m (z0, p0, -128))

/*
** dup_m129_s32_m:
**	mov	(z[0-9]+\.s), #-129
**	sel	z0\.s, p0, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (dup_m129_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, -129),
		z0 = svdup_s32_m (z0, p0, -129))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m130_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, -130),
		z0 = svdup_s32_m (z0, p0, -130))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m254_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, -254),
		z0 = svdup_s32_m (z0, p0, -254))

/*
** dup_m255_s32_m:
**	mov	(z[0-9]+\.s), #-255
**	sel	z0\.s, p0, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (dup_m255_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, -255),
		z0 = svdup_s32_m (z0, p0, -255))

/*
** dup_m256_s32_m:
**	mov	z0\.s, p0/m, #-256
**	ret
*/
TEST_UNIFORM_Z (dup_m256_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, -256),
		z0 = svdup_s32_m (z0, p0, -256))

/*
** dup_m257_s32_m:
**	mov	(z[0-9]+\.s), #-257
**	sel	z0\.s, p0, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (dup_m257_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, -257),
		z0 = svdup_s32_m (z0, p0, -257))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m258_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, -258),
		z0 = svdup_s32_m (z0, p0, -258))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m259_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, -259),
		z0 = svdup_s32_m (z0, p0, -259))

/*
** dup_m512_s32_m:
**	mov	z0\.s, p0/m, #-512
**	ret
*/
TEST_UNIFORM_Z (dup_m512_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, -512),
		z0 = svdup_s32_m (z0, p0, -512))

/*
** dup_m7f00_s32_m:
**	mov	z0\.s, p0/m, #-32512
**	ret
*/
TEST_UNIFORM_Z (dup_m7f00_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, -0x7f00),
		z0 = svdup_s32_m (z0, p0, -0x7f00))

/*
** dup_m7f01_s32_m:
**	mov	(z[0-9]+\.s), #-32513
**	sel	z0\.s, p0, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (dup_m7f01_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, -0x7f01),
		z0 = svdup_s32_m (z0, p0, -0x7f01))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m7f02_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, -0x7f02),
		z0 = svdup_s32_m (z0, p0, -0x7f02))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m7ffe_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, -0x7ffe),
		z0 = svdup_s32_m (z0, p0, -0x7ffe))

/*
** dup_m7fff_s32_m:
**	mov	(z[0-9]+\.s), #-32767
**	sel	z0\.s, p0, \1, z0\.s
**	ret
*/
TEST_UNIFORM_Z (dup_m7fff_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, -0x7fff),
		z0 = svdup_s32_m (z0, p0, -0x7fff))

/*
** dup_m8000_s32_m:
**	mov	z0\.s, p0/m, #-32768
**	ret
*/
TEST_UNIFORM_Z (dup_m8000_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, -0x8000),
		z0 = svdup_s32_m (z0, p0, -0x8000))

/*
** dup_0_s32_m:
**	mov	z0\.s, p0/m, #0
**	ret
*/
TEST_UNIFORM_Z (dup_0_s32_m, svint32_t,
		z0 = svdup_n_s32_m (z0, p0, 0),
		z0 = svdup_s32_m (z0, p0, 0))

/*
** dup_w0_s32_m:
**	movprfx	z0, z1
**	mov	z0\.s, p0/m, w0
**	ret
*/
TEST_UNIFORM_ZX (dup_w0_s32_m, svint32_t, int32_t,
		z0 = svdup_n_s32_m (z1, p0, x0),
		z0 = svdup_s32_m (z1, p0, x0))

/*
** dup_1_s32_z:
**	mov	z0\.s, p0/z, #1
**	ret
*/
TEST_UNIFORM_Z (dup_1_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, 1),
		z0 = svdup_s32_z (p0, 1))

/*
** dup_127_s32_z:
**	mov	z0\.s, p0/z, #127
**	ret
*/
TEST_UNIFORM_Z (dup_127_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, 127),
		z0 = svdup_s32_z (p0, 127))

/*
** dup_128_s32_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.s), #128
**	sel	z0\.s, p0, \2, \1\.s
** |
**	mov	(z[0-9]+\.s), #128
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.s, p0, \3, \4\.s
** )
**	ret
*/
TEST_UNIFORM_Z (dup_128_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, 128),
		z0 = svdup_s32_z (p0, 128))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_129_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, 129),
		z0 = svdup_s32_z (p0, 129))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_253_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, 253),
		z0 = svdup_s32_z (p0, 253))

/*
** dup_254_s32_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.s), #254
**	sel	z0\.s, p0, \2, \1\.s
** |
**	mov	(z[0-9]+\.s), #254
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.s, p0, \3, \4\.s
** )
**	ret
*/
TEST_UNIFORM_Z (dup_254_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, 254),
		z0 = svdup_s32_z (p0, 254))

/*
** dup_255_s32_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.s), #255
**	sel	z0\.s, p0, \2, \1\.s
** |
**	mov	(z[0-9]+\.s), #255
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.s, p0, \3, \4\.s
** )
**	ret
*/
TEST_UNIFORM_Z (dup_255_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, 255),
		z0 = svdup_s32_z (p0, 255))

/*
** dup_256_s32_z:
**	mov	z0\.s, p0/z, #256
**	ret
*/
TEST_UNIFORM_Z (dup_256_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, 256),
		z0 = svdup_s32_z (p0, 256))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_257_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, 257),
		z0 = svdup_s32_z (p0, 257))

/*
** dup_512_s32_z:
**	mov	z0\.s, p0/z, #512
**	ret
*/
TEST_UNIFORM_Z (dup_512_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, 512),
		z0 = svdup_s32_z (p0, 512))

/*
** dup_7f00_s32_z:
**	mov	z0\.s, p0/z, #32512
**	ret
*/
TEST_UNIFORM_Z (dup_7f00_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, 0x7f00),
		z0 = svdup_s32_z (p0, 0x7f00))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_7f01_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, 0x7f01),
		z0 = svdup_s32_z (p0, 0x7f01))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_7ffd_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, 0x7ffd),
		z0 = svdup_s32_z (p0, 0x7ffd))

/*
** dup_7ffe_s32_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.s), #32766
**	sel	z0\.s, p0, \2, \1\.s
** |
**	mov	(z[0-9]+\.s), #32766
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.s, p0, \3, \4\.s
** )
**	ret
*/
TEST_UNIFORM_Z (dup_7ffe_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, 0x7ffe),
		z0 = svdup_s32_z (p0, 0x7ffe))

/*
** dup_7fff_s32_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.s), #32767
**	sel	z0\.s, p0, \2, \1\.s
** |
**	mov	(z[0-9]+\.s), #32767
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.s, p0, \3, \4\.s
** )
**	ret
*/
TEST_UNIFORM_Z (dup_7fff_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, 0x7fff),
		z0 = svdup_s32_z (p0, 0x7fff))

/*
** dup_m1_s32_z:
**	mov	z0\.s, p0/z, #-1
**	ret
*/
TEST_UNIFORM_Z (dup_m1_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, -1),
		z0 = svdup_s32_z (p0, -1))

/*
** dup_m128_s32_z:
**	mov	z0\.s, p0/z, #-128
**	ret
*/
TEST_UNIFORM_Z (dup_m128_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, -128),
		z0 = svdup_s32_z (p0, -128))

/*
** dup_m129_s32_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.s), #-129
**	sel	z0\.s, p0, \2, \1\.s
** |
**	mov	(z[0-9]+\.s), #-129
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.s, p0, \3, \4\.s
** )
**	ret
*/
TEST_UNIFORM_Z (dup_m129_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, -129),
		z0 = svdup_s32_z (p0, -129))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m130_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, -130),
		z0 = svdup_s32_z (p0, -130))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m254_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, -254),
		z0 = svdup_s32_z (p0, -254))

/*
** dup_m255_s32_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.s), #-255
**	sel	z0\.s, p0, \2, \1\.s
** |
**	mov	(z[0-9]+\.s), #-255
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.s, p0, \3, \4\.s
** )
**	ret
*/
TEST_UNIFORM_Z (dup_m255_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, -255),
		z0 = svdup_s32_z (p0, -255))

/*
** dup_m256_s32_z:
**	mov	z0\.s, p0/z, #-256
**	ret
*/
TEST_UNIFORM_Z (dup_m256_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, -256),
		z0 = svdup_s32_z (p0, -256))

/*
** dup_m257_s32_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.s), #-257
**	sel	z0\.s, p0, \2, \1\.s
** |
**	mov	(z[0-9]+\.s), #-257
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.s, p0, \3, \4\.s
** )
**	ret
*/
TEST_UNIFORM_Z (dup_m257_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, -257),
		z0 = svdup_s32_z (p0, -257))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m258_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, -258),
		z0 = svdup_s32_z (p0, -258))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m259_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, -259),
		z0 = svdup_s32_z (p0, -259))

/*
** dup_m512_s32_z:
**	mov	z0\.s, p0/z, #-512
**	ret
*/
TEST_UNIFORM_Z (dup_m512_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, -512),
		z0 = svdup_s32_z (p0, -512))

/*
** dup_m7f00_s32_z:
**	mov	z0\.s, p0/z, #-32512
**	ret
*/
TEST_UNIFORM_Z (dup_m7f00_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, -0x7f00),
		z0 = svdup_s32_z (p0, -0x7f00))

/*
** dup_m7f01_s32_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.s), #-32513
**	sel	z0\.s, p0, \2, \1\.s
** |
**	mov	(z[0-9]+\.s), #-32513
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.s, p0, \3, \4\.s
** )
**	ret
*/
TEST_UNIFORM_Z (dup_m7f01_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, -0x7f01),
		z0 = svdup_s32_z (p0, -0x7f01))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m7f02_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, -0x7f02),
		z0 = svdup_s32_z (p0, -0x7f02))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m7ffe_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, -0x7ffe),
		z0 = svdup_s32_z (p0, -0x7ffe))

/*
** dup_m7fff_s32_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.s), #-32767
**	sel	z0\.s, p0, \2, \1\.s
** |
**	mov	(z[0-9]+\.s), #-32767
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.s, p0, \3, \4\.s
** )
**	ret
*/
TEST_UNIFORM_Z (dup_m7fff_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, -0x7fff),
		z0 = svdup_s32_z (p0, -0x7fff))

/*
** dup_m8000_s32_z:
**	mov	z0\.s, p0/z, #-32768
**	ret
*/
TEST_UNIFORM_Z (dup_m8000_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, -0x8000),
		z0 = svdup_s32_z (p0, -0x8000))

/*
** dup_0_s32_z:
**	mov	z0\.[bhsd], #0
**	ret
*/
TEST_UNIFORM_Z (dup_0_s32_z, svint32_t,
		z0 = svdup_n_s32_z (p0, 0),
		z0 = svdup_s32_z (p0, 0))

/*
** dup_w0_s32_z:
**	movprfx	z0\.s, p0/z, z0\.s
**	mov	z0\.s, p0/m, w0
**	ret
*/
TEST_UNIFORM_ZX (dup_w0_s32_z, svint32_t, int32_t,
		z0 = svdup_n_s32_z (p0, x0),
		z0 = svdup_s32_z (p0, x0))

/*
** dup_1_s32_x:
**	mov	z0\.s, #1
**	ret
*/
TEST_UNIFORM_Z (dup_1_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, 1),
		z0 = svdup_s32_x (p0, 1))

/*
** dup_127_s32_x:
**	mov	z0\.s, #127
**	ret
*/
TEST_UNIFORM_Z (dup_127_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, 127),
		z0 = svdup_s32_x (p0, 127))

/*
** dup_128_s32_x:
**	mov	z0\.s, #128
**	ret
*/
TEST_UNIFORM_Z (dup_128_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, 128),
		z0 = svdup_s32_x (p0, 128))

/*
** dup_129_s32_x:
**	movi	v([0-9]+)\.4s, 0x81
**	dup	z0\.q, z\1\.q\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_129_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, 129),
		z0 = svdup_s32_x (p0, 129))

/*
** dup_253_s32_x:
**	movi	v([0-9]+)\.4s, 0xfd
**	dup	z0\.q, z\1\.q\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_253_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, 253),
		z0 = svdup_s32_x (p0, 253))

/*
** dup_254_s32_x:
**	mov	z0\.s, #254
**	ret
*/
TEST_UNIFORM_Z (dup_254_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, 254),
		z0 = svdup_s32_x (p0, 254))

/*
** dup_255_s32_x:
**	mov	z0\.s, #255
**	ret
*/
TEST_UNIFORM_Z (dup_255_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, 255),
		z0 = svdup_s32_x (p0, 255))

/*
** dup_256_s32_x:
**	mov	z0\.s, #256
**	ret
*/
TEST_UNIFORM_Z (dup_256_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, 256),
		z0 = svdup_s32_x (p0, 256))

/*
** dup_257_s32_x:
**	mov	(w[0-9]+), 257
**	mov	z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (dup_257_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, 257),
		z0 = svdup_s32_x (p0, 257))

/*
** dup_512_s32_x:
**	mov	z0\.s, #512
**	ret
*/
TEST_UNIFORM_Z (dup_512_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, 512),
		z0 = svdup_s32_x (p0, 512))

/*
** dup_7f00_s32_x:
**	mov	z0\.s, #32512
**	ret
*/
TEST_UNIFORM_Z (dup_7f00_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, 0x7f00),
		z0 = svdup_s32_x (p0, 0x7f00))

/*
** dup_7f01_s32_x:
**	mov	(w[0-9]+), 32513
**	mov	z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (dup_7f01_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, 0x7f01),
		z0 = svdup_s32_x (p0, 0x7f01))

/*
** dup_7ffd_s32_x:
**	mov	(w[0-9]+), 32765
**	mov	z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (dup_7ffd_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, 0x7ffd),
		z0 = svdup_s32_x (p0, 0x7ffd))

/*
** dup_7ffe_s32_x:
**	mov	z0\.s, #32766
**	ret
*/
TEST_UNIFORM_Z (dup_7ffe_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, 0x7ffe),
		z0 = svdup_s32_x (p0, 0x7ffe))

/*
** dup_7fff_s32_x:
**	mov	z0\.s, #32767
**	ret
*/
TEST_UNIFORM_Z (dup_7fff_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, 0x7fff),
		z0 = svdup_s32_x (p0, 0x7fff))

/*
** dup_m1_s32_x:
**	mov	z0\.b, #-1
**	ret
*/
TEST_UNIFORM_Z (dup_m1_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, -1),
		z0 = svdup_s32_x (p0, -1))

/*
** dup_m128_s32_x:
**	mov	z0\.s, #-128
**	ret
*/
TEST_UNIFORM_Z (dup_m128_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, -128),
		z0 = svdup_s32_x (p0, -128))

/*
** dup_m129_s32_x:
**	mov	z0\.s, #-129
**	ret
*/
TEST_UNIFORM_Z (dup_m129_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, -129),
		z0 = svdup_s32_x (p0, -129))

/*
** dup_m130_s32_x:
**	mvni	v([0-9]+)\.4s, 0x81
**	dup	z0\.q, z\1\.q\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_m130_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, -130),
		z0 = svdup_s32_x (p0, -130))

/*
** dup_m254_s32_x:
**	mvni	v([0-9]+)\.4s, 0xfd
**	dup	z0\.q, z\1\.q\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_m254_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, -254),
		z0 = svdup_s32_x (p0, -254))

/*
** dup_m255_s32_x:
**	mov	z0\.s, #-255
**	ret
*/
TEST_UNIFORM_Z (dup_m255_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, -255),
		z0 = svdup_s32_x (p0, -255))

/*
** dup_m256_s32_x:
**	mov	z0\.s, #-256
**	ret
*/
TEST_UNIFORM_Z (dup_m256_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, -256),
		z0 = svdup_s32_x (p0, -256))

/*
** dup_m257_s32_x:
**	mov	z0\.s, #-257
**	ret
*/
TEST_UNIFORM_Z (dup_m257_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, -257),
		z0 = svdup_s32_x (p0, -257))

/*
** dup_m258_s32_x:
**	mov	(w[0-9]+), -258
**	mov	z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m258_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, -258),
		z0 = svdup_s32_x (p0, -258))

/*
** dup_m259_s32_x:
**	mov	(w[0-9]+), -259
**	mov	z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m259_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, -259),
		z0 = svdup_s32_x (p0, -259))

/*
** dup_m512_s32_x:
**	mov	z0\.s, #-512
**	ret
*/
TEST_UNIFORM_Z (dup_m512_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, -512),
		z0 = svdup_s32_x (p0, -512))

/*
** dup_m7f00_s32_x:
**	mov	z0\.s, #-32512
**	ret
*/
TEST_UNIFORM_Z (dup_m7f00_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, -0x7f00),
		z0 = svdup_s32_x (p0, -0x7f00))

/*
** dup_m7f01_s32_x:
**	mov	z0\.s, #-32513
**	ret
*/
TEST_UNIFORM_Z (dup_m7f01_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, -0x7f01),
		z0 = svdup_s32_x (p0, -0x7f01))

/*
** dup_m7f02_s32_x:
**	mov	(w[0-9]+), -32514
**	mov	z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m7f02_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, -0x7f02),
		z0 = svdup_s32_x (p0, -0x7f02))

/*
** dup_m7ffe_s32_x:
**	mov	(w[0-9]+), -32766
**	mov	z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m7ffe_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, -0x7ffe),
		z0 = svdup_s32_x (p0, -0x7ffe))

/*
** dup_m7fff_s32_x:
**	mov	z0\.s, #-32767
**	ret
*/
TEST_UNIFORM_Z (dup_m7fff_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, -0x7fff),
		z0 = svdup_s32_x (p0, -0x7fff))

/*
** dup_m8000_s32_x:
**	mov	z0\.s, #-32768
**	ret
*/
TEST_UNIFORM_Z (dup_m8000_s32_x, svint32_t,
		z0 = svdup_n_s32_x (p0, -0x8000),
		z0 = svdup_s32_x (p0, -0x8000))

/*
** dup_w0_s32_x:
**	mov	z0\.s, w0
**	ret
*/
TEST_UNIFORM_ZX (dup_w0_s32_x, svint32_t, int32_t,
		z0 = svdup_n_s32_x (p0, x0),
		z0 = svdup_s32_x (p0, x0))
