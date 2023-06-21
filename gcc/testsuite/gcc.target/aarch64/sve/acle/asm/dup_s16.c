/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dup_1_s16:
**	mov	z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (dup_1_s16, svint16_t,
		z0 = svdup_n_s16 (1),
		z0 = svdup_s16 (1))

/*
** dup_127_s16:
**	mov	z0\.h, #127
**	ret
*/
TEST_UNIFORM_Z (dup_127_s16, svint16_t,
		z0 = svdup_n_s16 (127),
		z0 = svdup_s16 (127))

/*
** dup_128_s16:
**	mov	z0\.h, #128
**	ret
*/
TEST_UNIFORM_Z (dup_128_s16, svint16_t,
		z0 = svdup_n_s16 (128),
		z0 = svdup_s16 (128))

/*
** dup_129_s16:
**	movi	v([0-9]+)\.8h, 0x81
**	dup	z0\.q, z\1\.q\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_129_s16, svint16_t,
		z0 = svdup_n_s16 (129),
		z0 = svdup_s16 (129))

/*
** dup_253_s16:
**	movi	v([0-9]+)\.8h, 0xfd
**	dup	z0\.q, z\1\.q\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_253_s16, svint16_t,
		z0 = svdup_n_s16 (253),
		z0 = svdup_s16 (253))

/*
** dup_254_s16:
**	mov	z0\.h, #254
**	ret
*/
TEST_UNIFORM_Z (dup_254_s16, svint16_t,
		z0 = svdup_n_s16 (254),
		z0 = svdup_s16 (254))

/*
** dup_255_s16:
**	mov	z0\.h, #255
**	ret
*/
TEST_UNIFORM_Z (dup_255_s16, svint16_t,
		z0 = svdup_n_s16 (255),
		z0 = svdup_s16 (255))

/*
** dup_256_s16:
**	mov	z0\.h, #256
**	ret
*/
TEST_UNIFORM_Z (dup_256_s16, svint16_t,
		z0 = svdup_n_s16 (256),
		z0 = svdup_s16 (256))

/*
** dup_257_s16:
**	mov	z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (dup_257_s16, svint16_t,
		z0 = svdup_n_s16 (257),
		z0 = svdup_s16 (257))

/*
** dup_512_s16:
**	mov	z0\.h, #512
**	ret
*/
TEST_UNIFORM_Z (dup_512_s16, svint16_t,
		z0 = svdup_n_s16 (512),
		z0 = svdup_s16 (512))

/*
** dup_7f00_s16:
**	mov	z0\.h, #32512
**	ret
*/
TEST_UNIFORM_Z (dup_7f00_s16, svint16_t,
		z0 = svdup_n_s16 (0x7f00),
		z0 = svdup_s16 (0x7f00))

/*
** dup_7f01_s16:
**	mov	(w[0-9]+), 32513
**	mov	z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (dup_7f01_s16, svint16_t,
		z0 = svdup_n_s16 (0x7f01),
		z0 = svdup_s16 (0x7f01))

/*
** dup_7ffd_s16:
**	mov	(w[0-9]+), 32765
**	mov	z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (dup_7ffd_s16, svint16_t,
		z0 = svdup_n_s16 (0x7ffd),
		z0 = svdup_s16 (0x7ffd))

/*
** dup_7ffe_s16:
**	mov	z0\.h, #32766
**	ret
*/
TEST_UNIFORM_Z (dup_7ffe_s16, svint16_t,
		z0 = svdup_n_s16 (0x7ffe),
		z0 = svdup_s16 (0x7ffe))

/*
** dup_7fff_s16:
**	mov	z0\.h, #32767
**	ret
*/
TEST_UNIFORM_Z (dup_7fff_s16, svint16_t,
		z0 = svdup_n_s16 (0x7fff),
		z0 = svdup_s16 (0x7fff))

/*
** dup_m1_s16:
**	mov	z0\.b, #-1
**	ret
*/
TEST_UNIFORM_Z (dup_m1_s16, svint16_t,
		z0 = svdup_n_s16 (-1),
		z0 = svdup_s16 (-1))

/*
** dup_m128_s16:
**	mov	z0\.h, #-128
**	ret
*/
TEST_UNIFORM_Z (dup_m128_s16, svint16_t,
		z0 = svdup_n_s16 (-128),
		z0 = svdup_s16 (-128))

/*
** dup_m129_s16:
**	mov	z0\.h, #-129
**	ret
*/
TEST_UNIFORM_Z (dup_m129_s16, svint16_t,
		z0 = svdup_n_s16 (-129),
		z0 = svdup_s16 (-129))

/*
** dup_m130_s16:
**	mvni	v([0-9]+)\.8h, 0x81
**	dup	z0\.q, z\1\.q\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_m130_s16, svint16_t,
		z0 = svdup_n_s16 (-130),
		z0 = svdup_s16 (-130))

/*
** dup_m254_s16:
**	mvni	v([0-9]+)\.8h, 0xfd
**	dup	z0\.q, z\1\.q\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_m254_s16, svint16_t,
		z0 = svdup_n_s16 (-254),
		z0 = svdup_s16 (-254))

/*
** dup_m255_s16:
**	mov	z0\.h, #-255
**	ret
*/
TEST_UNIFORM_Z (dup_m255_s16, svint16_t,
		z0 = svdup_n_s16 (-255),
		z0 = svdup_s16 (-255))

/*
** dup_m256_s16:
**	mov	z0\.h, #-256
**	ret
*/
TEST_UNIFORM_Z (dup_m256_s16, svint16_t,
		z0 = svdup_n_s16 (-256),
		z0 = svdup_s16 (-256))

/*
** dup_m257_s16:
**	mov	z0\.h, #-257
**	ret
*/
TEST_UNIFORM_Z (dup_m257_s16, svint16_t,
		z0 = svdup_n_s16 (-257),
		z0 = svdup_s16 (-257))

/*
** dup_m258_s16:
**	mov	z0\.b, #-2
**	ret
*/
TEST_UNIFORM_Z (dup_m258_s16, svint16_t,
		z0 = svdup_n_s16 (-258),
		z0 = svdup_s16 (-258))

/*
** dup_m259_s16:
**	mov	(w[0-9]+), -259
**	mov	z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m259_s16, svint16_t,
		z0 = svdup_n_s16 (-259),
		z0 = svdup_s16 (-259))

/*
** dup_m512_s16:
**	mov	z0\.h, #-512
**	ret
*/
TEST_UNIFORM_Z (dup_m512_s16, svint16_t,
		z0 = svdup_n_s16 (-512),
		z0 = svdup_s16 (-512))

/*
** dup_m7f00_s16:
**	mov	z0\.h, #-32512
**	ret
*/
TEST_UNIFORM_Z (dup_m7f00_s16, svint16_t,
		z0 = svdup_n_s16 (-0x7f00),
		z0 = svdup_s16 (-0x7f00))

/*
** dup_m7f01_s16:
**	mov	z0\.h, #-32513
**	ret
*/
TEST_UNIFORM_Z (dup_m7f01_s16, svint16_t,
		z0 = svdup_n_s16 (-0x7f01),
		z0 = svdup_s16 (-0x7f01))

/*
** dup_m7f02_s16:
**	mov	(w[0-9]+), -32514
**	mov	z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m7f02_s16, svint16_t,
		z0 = svdup_n_s16 (-0x7f02),
		z0 = svdup_s16 (-0x7f02))

/*
** dup_m7ffe_s16:
**	mov	(w[0-9]+), -32766
**	mov	z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m7ffe_s16, svint16_t,
		z0 = svdup_n_s16 (-0x7ffe),
		z0 = svdup_s16 (-0x7ffe))

/*
** dup_m7fff_s16:
**	mov	z0\.h, #-32767
**	ret
*/
TEST_UNIFORM_Z (dup_m7fff_s16, svint16_t,
		z0 = svdup_n_s16 (-0x7fff),
		z0 = svdup_s16 (-0x7fff))

/*
** dup_m8000_s16:
**	mov	z0\.h, #-32768
**	ret
*/
TEST_UNIFORM_Z (dup_m8000_s16, svint16_t,
		z0 = svdup_n_s16 (-0x8000),
		z0 = svdup_s16 (-0x8000))

/*
** dup_w0_s16:
**	mov	z0\.h, w0
**	ret
*/
TEST_UNIFORM_ZX (dup_w0_s16, svint16_t, int16_t,
		 z0 = svdup_n_s16 (x0),
		 z0 = svdup_s16 (x0))

/*
** dup_1_s16_m:
**	mov	z0\.h, p0/m, #1
**	ret
*/
TEST_UNIFORM_Z (dup_1_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, 1),
		z0 = svdup_s16_m (z0, p0, 1))

/*
** dup_127_s16_m:
**	mov	z0\.h, p0/m, #127
**	ret
*/
TEST_UNIFORM_Z (dup_127_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, 127),
		z0 = svdup_s16_m (z0, p0, 127))

/*
** dup_128_s16_m:
**	mov	(z[0-9]+\.h), #128
**	sel	z0\.h, p0, \1, z0\.h
**	ret
*/
TEST_UNIFORM_Z (dup_128_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, 128),
		z0 = svdup_s16_m (z0, p0, 128))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_129_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, 129),
		z0 = svdup_s16_m (z0, p0, 129))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_253_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, 253),
		z0 = svdup_s16_m (z0, p0, 253))

/*
** dup_254_s16_m:
**	mov	(z[0-9]+\.h), #254
**	sel	z0\.h, p0, \1, z0\.h
**	ret
*/
TEST_UNIFORM_Z (dup_254_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, 254),
		z0 = svdup_s16_m (z0, p0, 254))

/*
** dup_255_s16_m:
**	mov	(z[0-9]+\.h), #255
**	sel	z0\.h, p0, \1, z0\.h
**	ret
*/
TEST_UNIFORM_Z (dup_255_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, 255),
		z0 = svdup_s16_m (z0, p0, 255))

/*
** dup_256_s16_m:
**	mov	z0\.h, p0/m, #256
**	ret
*/
TEST_UNIFORM_Z (dup_256_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, 256),
		z0 = svdup_s16_m (z0, p0, 256))

/*
** dup_257_s16_m:
**	mov	(z[0-9]+)\.b, #1
**	sel	z0\.h, p0, \1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (dup_257_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, 257),
		z0 = svdup_s16_m (z0, p0, 257))

/*
** dup_512_s16_m:
**	mov	z0\.h, p0/m, #512
**	ret
*/
TEST_UNIFORM_Z (dup_512_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, 512),
		z0 = svdup_s16_m (z0, p0, 512))

/*
** dup_7f00_s16_m:
**	mov	z0\.h, p0/m, #32512
**	ret
*/
TEST_UNIFORM_Z (dup_7f00_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, 0x7f00),
		z0 = svdup_s16_m (z0, p0, 0x7f00))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_7f01_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, 0x7f01),
		z0 = svdup_s16_m (z0, p0, 0x7f01))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_7ffd_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, 0x7ffd),
		z0 = svdup_s16_m (z0, p0, 0x7ffd))

/*
** dup_7ffe_s16_m:
**	mov	(z[0-9]+\.h), #32766
**	sel	z0\.h, p0, \1, z0\.h
**	ret
*/
TEST_UNIFORM_Z (dup_7ffe_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, 0x7ffe),
		z0 = svdup_s16_m (z0, p0, 0x7ffe))

/*
** dup_7fff_s16_m:
**	mov	(z[0-9]+\.h), #32767
**	sel	z0\.h, p0, \1, z0\.h
**	ret
*/
TEST_UNIFORM_Z (dup_7fff_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, 0x7fff),
		z0 = svdup_s16_m (z0, p0, 0x7fff))

/*
** dup_m1_s16_m:
**	mov	z0\.h, p0/m, #-1
**	ret
*/
TEST_UNIFORM_Z (dup_m1_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, -1),
		z0 = svdup_s16_m (z0, p0, -1))

/*
** dup_m128_s16_m:
**	mov	z0\.h, p0/m, #-128
**	ret
*/
TEST_UNIFORM_Z (dup_m128_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, -128),
		z0 = svdup_s16_m (z0, p0, -128))

/*
** dup_m129_s16_m:
**	mov	(z[0-9]+\.h), #-129
**	sel	z0\.h, p0, \1, z0\.h
**	ret
*/
TEST_UNIFORM_Z (dup_m129_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, -129),
		z0 = svdup_s16_m (z0, p0, -129))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m130_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, -130),
		z0 = svdup_s16_m (z0, p0, -130))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m254_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, -254),
		z0 = svdup_s16_m (z0, p0, -254))

/*
** dup_m255_s16_m:
**	mov	(z[0-9]+\.h), #-255
**	sel	z0\.h, p0, \1, z0\.h
**	ret
*/
TEST_UNIFORM_Z (dup_m255_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, -255),
		z0 = svdup_s16_m (z0, p0, -255))

/*
** dup_m256_s16_m:
**	mov	z0\.h, p0/m, #-256
**	ret
*/
TEST_UNIFORM_Z (dup_m256_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, -256),
		z0 = svdup_s16_m (z0, p0, -256))

/*
** dup_m257_s16_m:
**	mov	(z[0-9]+\.h), #-257
**	sel	z0\.h, p0, \1, z0\.h
**	ret
*/
TEST_UNIFORM_Z (dup_m257_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, -257),
		z0 = svdup_s16_m (z0, p0, -257))

/*
** dup_m258_s16_m:
**	mov	(z[0-9]+)\.b, #-2
**	sel	z0\.h, p0, \1\.h, z0\.h
**	ret
*/
TEST_UNIFORM_Z (dup_m258_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, -258),
		z0 = svdup_s16_m (z0, p0, -258))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m259_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, -259),
		z0 = svdup_s16_m (z0, p0, -259))

/*
** dup_m512_s16_m:
**	mov	z0\.h, p0/m, #-512
**	ret
*/
TEST_UNIFORM_Z (dup_m512_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, -512),
		z0 = svdup_s16_m (z0, p0, -512))

/*
** dup_m7f00_s16_m:
**	mov	z0\.h, p0/m, #-32512
**	ret
*/
TEST_UNIFORM_Z (dup_m7f00_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, -0x7f00),
		z0 = svdup_s16_m (z0, p0, -0x7f00))

/*
** dup_m7f01_s16_m:
**	mov	(z[0-9]+\.h), #-32513
**	sel	z0\.h, p0, \1, z0\.h
**	ret
*/
TEST_UNIFORM_Z (dup_m7f01_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, -0x7f01),
		z0 = svdup_s16_m (z0, p0, -0x7f01))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m7f02_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, -0x7f02),
		z0 = svdup_s16_m (z0, p0, -0x7f02))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m7ffe_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, -0x7ffe),
		z0 = svdup_s16_m (z0, p0, -0x7ffe))

/*
** dup_m7fff_s16_m:
**	mov	(z[0-9]+\.h), #-32767
**	sel	z0\.h, p0, \1, z0\.h
**	ret
*/
TEST_UNIFORM_Z (dup_m7fff_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, -0x7fff),
		z0 = svdup_s16_m (z0, p0, -0x7fff))

/*
** dup_m8000_s16_m:
**	mov	z0\.h, p0/m, #-32768
**	ret
*/
TEST_UNIFORM_Z (dup_m8000_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, -0x8000),
		z0 = svdup_s16_m (z0, p0, -0x8000))

/*
** dup_0_s16_m:
**	mov	z0\.h, p0/m, #0
**	ret
*/
TEST_UNIFORM_Z (dup_0_s16_m, svint16_t,
		z0 = svdup_n_s16_m (z0, p0, 0),
		z0 = svdup_s16_m (z0, p0, 0))

/*
** dup_w0_s16_m:
**	movprfx	z0, z1
**	mov	z0\.h, p0/m, w0
**	ret
*/
TEST_UNIFORM_ZX (dup_w0_s16_m, svint16_t, int16_t,
		z0 = svdup_n_s16_m (z1, p0, x0),
		z0 = svdup_s16_m (z1, p0, x0))

/*
** dup_1_s16_z:
**	mov	z0\.h, p0/z, #1
**	ret
*/
TEST_UNIFORM_Z (dup_1_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, 1),
		z0 = svdup_s16_z (p0, 1))

/*
** dup_127_s16_z:
**	mov	z0\.h, p0/z, #127
**	ret
*/
TEST_UNIFORM_Z (dup_127_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, 127),
		z0 = svdup_s16_z (p0, 127))

/*
** dup_128_s16_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.h), #128
**	sel	z0\.h, p0, \2, \1\.h
** |
**	mov	(z[0-9]+\.h), #128
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.h, p0, \3, \4\.h
** )
**	ret
*/
TEST_UNIFORM_Z (dup_128_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, 128),
		z0 = svdup_s16_z (p0, 128))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_129_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, 129),
		z0 = svdup_s16_z (p0, 129))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_253_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, 253),
		z0 = svdup_s16_z (p0, 253))

/*
** dup_254_s16_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.h), #254
**	sel	z0\.h, p0, \2, \1\.h
** |
**	mov	(z[0-9]+\.h), #254
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.h, p0, \3, \4\.h
** )
**	ret
*/
TEST_UNIFORM_Z (dup_254_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, 254),
		z0 = svdup_s16_z (p0, 254))

/*
** dup_255_s16_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.h), #255
**	sel	z0\.h, p0, \2, \1\.h
** |
**	mov	(z[0-9]+\.h), #255
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.h, p0, \3, \4\.h
** )
**	ret
*/
TEST_UNIFORM_Z (dup_255_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, 255),
		z0 = svdup_s16_z (p0, 255))

/*
** dup_256_s16_z:
**	mov	z0\.h, p0/z, #256
**	ret
*/
TEST_UNIFORM_Z (dup_256_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, 256),
		z0 = svdup_s16_z (p0, 256))

/*
** dup_257_s16_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+)\.b, #1
**	sel	z0\.h, p0, \2\.h, \1\.h
** |
**	mov	(z[0-9]+)\.b, #1
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.h, p0, \3\.h, \4\.h
** )
**	ret
*/
TEST_UNIFORM_Z (dup_257_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, 257),
		z0 = svdup_s16_z (p0, 257))

/*
** dup_512_s16_z:
**	mov	z0\.h, p0/z, #512
**	ret
*/
TEST_UNIFORM_Z (dup_512_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, 512),
		z0 = svdup_s16_z (p0, 512))

/*
** dup_7f00_s16_z:
**	mov	z0\.h, p0/z, #32512
**	ret
*/
TEST_UNIFORM_Z (dup_7f00_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, 0x7f00),
		z0 = svdup_s16_z (p0, 0x7f00))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_7f01_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, 0x7f01),
		z0 = svdup_s16_z (p0, 0x7f01))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_7ffd_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, 0x7ffd),
		z0 = svdup_s16_z (p0, 0x7ffd))

/*
** dup_7ffe_s16_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.h), #32766
**	sel	z0\.h, p0, \2, \1\.h
** |
**	mov	(z[0-9]+\.h), #32766
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.h, p0, \3, \4\.h
** )
**	ret
*/
TEST_UNIFORM_Z (dup_7ffe_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, 0x7ffe),
		z0 = svdup_s16_z (p0, 0x7ffe))

/*
** dup_7fff_s16_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.h), #32767
**	sel	z0\.h, p0, \2, \1\.h
** |
**	mov	(z[0-9]+\.h), #32767
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.h, p0, \3, \4\.h
** )
**	ret
*/
TEST_UNIFORM_Z (dup_7fff_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, 0x7fff),
		z0 = svdup_s16_z (p0, 0x7fff))

/*
** dup_m1_s16_z:
**	mov	z0\.h, p0/z, #-1
**	ret
*/
TEST_UNIFORM_Z (dup_m1_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, -1),
		z0 = svdup_s16_z (p0, -1))

/*
** dup_m128_s16_z:
**	mov	z0\.h, p0/z, #-128
**	ret
*/
TEST_UNIFORM_Z (dup_m128_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, -128),
		z0 = svdup_s16_z (p0, -128))

/*
** dup_m129_s16_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.h), #-129
**	sel	z0\.h, p0, \2, \1\.h
** |
**	mov	(z[0-9]+\.h), #-129
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.h, p0, \3, \4\.h
** )
**	ret
*/
TEST_UNIFORM_Z (dup_m129_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, -129),
		z0 = svdup_s16_z (p0, -129))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m130_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, -130),
		z0 = svdup_s16_z (p0, -130))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m254_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, -254),
		z0 = svdup_s16_z (p0, -254))

/*
** dup_m255_s16_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.h), #-255
**	sel	z0\.h, p0, \2, \1\.h
** |
**	mov	(z[0-9]+\.h), #-255
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.h, p0, \3, \4\.h
** )
**	ret
*/
TEST_UNIFORM_Z (dup_m255_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, -255),
		z0 = svdup_s16_z (p0, -255))

/*
** dup_m256_s16_z:
**	mov	z0\.h, p0/z, #-256
**	ret
*/
TEST_UNIFORM_Z (dup_m256_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, -256),
		z0 = svdup_s16_z (p0, -256))

/*
** dup_m257_s16_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.h), #-257
**	sel	z0\.h, p0, \2, \1\.h
** |
**	mov	(z[0-9]+\.h), #-257
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.h, p0, \3, \4\.h
** )
**	ret
*/
TEST_UNIFORM_Z (dup_m257_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, -257),
		z0 = svdup_s16_z (p0, -257))

/*
** dup_m258_s16_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+)\.b, #-2
**	sel	z0\.h, p0, \2\.h, \1\.h
** |
**	mov	(z[0-9]+)\.b, #-2
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.h, p0, \3\.h, \4\.h
** )
**	ret
*/
TEST_UNIFORM_Z (dup_m258_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, -258),
		z0 = svdup_s16_z (p0, -258))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m259_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, -259),
		z0 = svdup_s16_z (p0, -259))

/*
** dup_m512_s16_z:
**	mov	z0\.h, p0/z, #-512
**	ret
*/
TEST_UNIFORM_Z (dup_m512_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, -512),
		z0 = svdup_s16_z (p0, -512))

/*
** dup_m7f00_s16_z:
**	mov	z0\.h, p0/z, #-32512
**	ret
*/
TEST_UNIFORM_Z (dup_m7f00_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, -0x7f00),
		z0 = svdup_s16_z (p0, -0x7f00))

/*
** dup_m7f01_s16_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.h), #-32513
**	sel	z0\.h, p0, \2, \1\.h
** |
**	mov	(z[0-9]+\.h), #-32513
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.h, p0, \3, \4\.h
** )
**	ret
*/
TEST_UNIFORM_Z (dup_m7f01_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, -0x7f01),
		z0 = svdup_s16_z (p0, -0x7f01))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m7f02_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, -0x7f02),
		z0 = svdup_s16_z (p0, -0x7f02))

/* TODO: Bad code and needs fixing.  */
TEST_UNIFORM_Z (dup_m7ffe_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, -0x7ffe),
		z0 = svdup_s16_z (p0, -0x7ffe))

/*
** dup_m7fff_s16_z:
** (
**	mov	(z[0-9]+)\.b, #0
**	mov	(z[0-9]+\.h), #-32767
**	sel	z0\.h, p0, \2, \1\.h
** |
**	mov	(z[0-9]+\.h), #-32767
**	mov	(z[0-9]+)\.b, #0
**	sel	z0\.h, p0, \3, \4\.h
** )
**	ret
*/
TEST_UNIFORM_Z (dup_m7fff_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, -0x7fff),
		z0 = svdup_s16_z (p0, -0x7fff))

/*
** dup_m8000_s16_z:
**	mov	z0\.h, p0/z, #-32768
**	ret
*/
TEST_UNIFORM_Z (dup_m8000_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, -0x8000),
		z0 = svdup_s16_z (p0, -0x8000))

/*
** dup_0_s16_z:
**	mov	z0\.[bhsd], #0
**	ret
*/
TEST_UNIFORM_Z (dup_0_s16_z, svint16_t,
		z0 = svdup_n_s16_z (p0, 0),
		z0 = svdup_s16_z (p0, 0))

/*
** dup_w0_s16_z:
**	movprfx	z0\.h, p0/z, z0\.h
**	mov	z0\.h, p0/m, w0
**	ret
*/
TEST_UNIFORM_ZX (dup_w0_s16_z, svint16_t, int16_t,
		z0 = svdup_n_s16_z (p0, x0),
		z0 = svdup_s16_z (p0, x0))

/*
** dup_1_s16_x:
**	mov	z0\.h, #1
**	ret
*/
TEST_UNIFORM_Z (dup_1_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, 1),
		z0 = svdup_s16_x (p0, 1))

/*
** dup_127_s16_x:
**	mov	z0\.h, #127
**	ret
*/
TEST_UNIFORM_Z (dup_127_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, 127),
		z0 = svdup_s16_x (p0, 127))

/*
** dup_128_s16_x:
**	mov	z0\.h, #128
**	ret
*/
TEST_UNIFORM_Z (dup_128_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, 128),
		z0 = svdup_s16_x (p0, 128))

/*
** dup_129_s16_x:
**	movi	v([0-9]+)\.8h, 0x81
**	dup	z0\.q, z\1\.q\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_129_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, 129),
		z0 = svdup_s16_x (p0, 129))

/*
** dup_253_s16_x:
**	movi	v([0-9]+)\.8h, 0xfd
**	dup	z0\.q, z\1\.q\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_253_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, 253),
		z0 = svdup_s16_x (p0, 253))

/*
** dup_254_s16_x:
**	mov	z0\.h, #254
**	ret
*/
TEST_UNIFORM_Z (dup_254_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, 254),
		z0 = svdup_s16_x (p0, 254))

/*
** dup_255_s16_x:
**	mov	z0\.h, #255
**	ret
*/
TEST_UNIFORM_Z (dup_255_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, 255),
		z0 = svdup_s16_x (p0, 255))

/*
** dup_256_s16_x:
**	mov	z0\.h, #256
**	ret
*/
TEST_UNIFORM_Z (dup_256_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, 256),
		z0 = svdup_s16_x (p0, 256))

/*
** dup_257_s16_x:
**	mov	z0\.b, #1
**	ret
*/
TEST_UNIFORM_Z (dup_257_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, 257),
		z0 = svdup_s16_x (p0, 257))

/*
** dup_512_s16_x:
**	mov	z0\.h, #512
**	ret
*/
TEST_UNIFORM_Z (dup_512_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, 512),
		z0 = svdup_s16_x (p0, 512))

/*
** dup_7f00_s16_x:
**	mov	z0\.h, #32512
**	ret
*/
TEST_UNIFORM_Z (dup_7f00_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, 0x7f00),
		z0 = svdup_s16_x (p0, 0x7f00))

/*
** dup_7f01_s16_x:
**	mov	(w[0-9]+), 32513
**	mov	z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (dup_7f01_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, 0x7f01),
		z0 = svdup_s16_x (p0, 0x7f01))

/*
** dup_7ffd_s16_x:
**	mov	(w[0-9]+), 32765
**	mov	z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (dup_7ffd_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, 0x7ffd),
		z0 = svdup_s16_x (p0, 0x7ffd))

/*
** dup_7ffe_s16_x:
**	mov	z0\.h, #32766
**	ret
*/
TEST_UNIFORM_Z (dup_7ffe_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, 0x7ffe),
		z0 = svdup_s16_x (p0, 0x7ffe))

/*
** dup_7fff_s16_x:
**	mov	z0\.h, #32767
**	ret
*/
TEST_UNIFORM_Z (dup_7fff_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, 0x7fff),
		z0 = svdup_s16_x (p0, 0x7fff))

/*
** dup_m1_s16_x:
**	mov	z0\.b, #-1
**	ret
*/
TEST_UNIFORM_Z (dup_m1_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, -1),
		z0 = svdup_s16_x (p0, -1))

/*
** dup_m128_s16_x:
**	mov	z0\.h, #-128
**	ret
*/
TEST_UNIFORM_Z (dup_m128_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, -128),
		z0 = svdup_s16_x (p0, -128))

/*
** dup_m129_s16_x:
**	mov	z0\.h, #-129
**	ret
*/
TEST_UNIFORM_Z (dup_m129_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, -129),
		z0 = svdup_s16_x (p0, -129))

/*
** dup_m130_s16_x:
**	mvni	v([0-9]+)\.8h, 0x81
**	dup	z0\.q, z\1\.q\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_m130_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, -130),
		z0 = svdup_s16_x (p0, -130))

/*
** dup_m254_s16_x:
**	mvni	v([0-9]+)\.8h, 0xfd
**	dup	z0\.q, z\1\.q\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_m254_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, -254),
		z0 = svdup_s16_x (p0, -254))

/*
** dup_m255_s16_x:
**	mov	z0\.h, #-255
**	ret
*/
TEST_UNIFORM_Z (dup_m255_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, -255),
		z0 = svdup_s16_x (p0, -255))

/*
** dup_m256_s16_x:
**	mov	z0\.h, #-256
**	ret
*/
TEST_UNIFORM_Z (dup_m256_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, -256),
		z0 = svdup_s16_x (p0, -256))

/*
** dup_m257_s16_x:
**	mov	z0\.h, #-257
**	ret
*/
TEST_UNIFORM_Z (dup_m257_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, -257),
		z0 = svdup_s16_x (p0, -257))

/*
** dup_m258_s16_x:
**	mov	z0\.b, #-2
**	ret
*/
TEST_UNIFORM_Z (dup_m258_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, -258),
		z0 = svdup_s16_x (p0, -258))

/*
** dup_m259_s16_x:
**	mov	(w[0-9]+), -259
**	mov	z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m259_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, -259),
		z0 = svdup_s16_x (p0, -259))

/*
** dup_m512_s16_x:
**	mov	z0\.h, #-512
**	ret
*/
TEST_UNIFORM_Z (dup_m512_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, -512),
		z0 = svdup_s16_x (p0, -512))

/*
** dup_m7f00_s16_x:
**	mov	z0\.h, #-32512
**	ret
*/
TEST_UNIFORM_Z (dup_m7f00_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, -0x7f00),
		z0 = svdup_s16_x (p0, -0x7f00))

/*
** dup_m7f01_s16_x:
**	mov	z0\.h, #-32513
**	ret
*/
TEST_UNIFORM_Z (dup_m7f01_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, -0x7f01),
		z0 = svdup_s16_x (p0, -0x7f01))

/*
** dup_m7f02_s16_x:
**	mov	(w[0-9]+), -32514
**	mov	z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m7f02_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, -0x7f02),
		z0 = svdup_s16_x (p0, -0x7f02))

/*
** dup_m7ffe_s16_x:
**	mov	(w[0-9]+), -32766
**	mov	z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (dup_m7ffe_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, -0x7ffe),
		z0 = svdup_s16_x (p0, -0x7ffe))

/*
** dup_m7fff_s16_x:
**	mov	z0\.h, #-32767
**	ret
*/
TEST_UNIFORM_Z (dup_m7fff_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, -0x7fff),
		z0 = svdup_s16_x (p0, -0x7fff))

/*
** dup_m8000_s16_x:
**	mov	z0\.h, #-32768
**	ret
*/
TEST_UNIFORM_Z (dup_m8000_s16_x, svint16_t,
		z0 = svdup_n_s16_x (p0, -0x8000),
		z0 = svdup_s16_x (p0, -0x8000))

/*
** dup_w0_s16_x:
**	mov	z0\.h, w0
**	ret
*/
TEST_UNIFORM_ZX (dup_w0_s16_x, svint16_t, int16_t,
		z0 = svdup_n_s16_x (p0, x0),
		z0 = svdup_s16_x (p0, x0))
