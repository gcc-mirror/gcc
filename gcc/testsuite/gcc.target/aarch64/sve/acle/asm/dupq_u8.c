/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dupq_54h_u8:
**	mov	z0\.h, #54
**	ret
*/
TEST_UNIFORM_Z (dupq_54h_u8, svuint8_t,
		z0 = svdupq_n_u8 (54, 0, 54, 0, 54, 0, 54, 0,
				  54, 0, 54, 0, 54, 0, 54, 0),
		z0 = svdupq_u8 (54, 0, 54, 0, 54, 0, 54, 0,
				54, 0, 54, 0, 54, 0, 54, 0))

/*
** dupq_2560h_u8:
**	mov	z0\.h, #2560
**	ret
*/
TEST_UNIFORM_Z (dupq_2560h_u8, svuint8_t,
		z0 = svdupq_n_u8 (0, 10, 0, 10, 0, 10, 0, 10,
				  0, 10, 0, 10, 0, 10, 0, 10),
		z0 = svdupq_u8 (0, 10, 0, 10, 0, 10, 0, 10,
				0, 10, 0, 10, 0, 10, 0, 10))

/*
** dupq_5120s_u8:
**	mov	z0\.s, #5120
**	ret
*/
TEST_UNIFORM_Z (dupq_5120s_u8, svuint8_t,
		z0 = svdupq_n_u8 (0, 20, 0, 0, 0, 20, 0, 0,
				  0, 20, 0, 0, 0, 20, 0, 0),
		z0 = svdupq_u8 (0, 20, 0, 0, 0, 20, 0, 0,
				0, 20, 0, 0, 0, 20, 0, 0))

/*
** dupq_1ff00s_u8:
**	mov	z0\.s, #130816
**	ret
*/
TEST_UNIFORM_Z (dupq_1ff00s_u8, svuint8_t,
		z0 = svdupq_n_u8 (0, -1, 1, 0, 0, -1, 1, 0,
				  0, -1, 1, 0, 0, -1, 1, 0),
		z0 = svdupq_u8 (0, -1, 1, 0, 0, -1, 1, 0,
				0, -1, 1, 0, 0, -1, 1, 0))

/*
** dupq_96db_u8:
**	movi	v([0-9]+)\.2d, 0xff0000ff00ffff00
**	dup	z0\.q, z\1\.q\[0\]
**	ret
*/
TEST_UNIFORM_Z (dupq_96db_u8, svuint8_t,
		z0 = svdupq_n_u8 (0, -1, -1, 0, -1, 0, 0, -1,
				  0, -1, -1, 0, -1, 0, 0, -1),
		z0 = svdupq_u8 (0, -1, -1, 0, -1, 0, 0, -1,
				0, -1, -1, 0, -1, 0, 0, -1))

/*
** dupq_7755h_u8:
**	mov	(w[0-9]+), 21879
**	mov	z0\.h, \1
**	ret
*/
TEST_UNIFORM_Z (dupq_7755h_u8, svuint8_t,
		z0 = svdupq_n_u8 (0x77, 0x55, 0x77, 0x55,
				  0x77, 0x55, 0x77, 0x55,
				  0x77, 0x55, 0x77, 0x55,
				  0x77, 0x55, 0x77, 0x55),
		z0 = svdupq_u8 (0x77, 0x55, 0x77, 0x55,
				0x77, 0x55, 0x77, 0x55,
				0x77, 0x55, 0x77, 0x55,
				0x77, 0x55, 0x77, 0x55))

/*
** dupq_729a0000s_u8:
**	mov	(w[0-9]+), 1922695168
**	mov	z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (dupq_729a0000s_u8, svuint8_t,
		z0 = svdupq_n_u8 (0, 0, 0x9a, 0x72, 0, 0, 0x9a, 0x72,
				  0, 0, 0x9a, 0x72, 0, 0, 0x9a, 0x72),
		z0 = svdupq_u8 (0, 0, 0x9a, 0x72, 0, 0, 0x9a, 0x72,
				0, 0, 0x9a, 0x72, 0, 0, 0x9a, 0x72))

/*
** dupq_pool_u8:
**	...
**	ld1rqb	z0\.b, p[0-7]/z, \[x[0-9]+\]
**	ret
*/
TEST_UNIFORM_Z (dupq_pool_u8, svuint8_t,
		z0 = svdupq_n_u8 (4, 10, 9, 77, 52, 22, 19, 50,
				  -1, 32, 44, 17, 23, 99, 53, 39),
		z0 = svdupq_u8 (4, 10, 9, 77, 52, 22, 19, 50,
				-1, 32, 44, 17, 23, 99, 53, 39))
