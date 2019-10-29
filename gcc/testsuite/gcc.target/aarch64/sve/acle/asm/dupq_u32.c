/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dupq_12800d_u32:
**	mov	z0\.d, #12800
**	ret
*/
TEST_UNIFORM_Z (dupq_12800d_u32, svuint32_t,
		z0 = svdupq_n_u32 (12800, 0, 12800, 0),
		z0 = svdupq_u32 (12800, 0, 12800, 0))

/*
** dupq_fffffffed_u32:
**	mov	z0\.d, #4294967294
**	ret
*/
TEST_UNIFORM_Z (dupq_fffffffed_u32, svuint32_t,
		z0 = svdupq_n_u32 (-2, 0, -2, 0),
		z0 = svdupq_u32 (-2, 0, -2, 0))

/*
** dupq_ff00ffffff00d_u32:
**	movi	v([0-9]+)\.2d, 0xff00ffffff00
**	dup	z0\.q, z\1\.q\[0\]
**	ret
*/
TEST_UNIFORM_Z (dupq_ff00ffffff00d_u32, svuint32_t,
		z0 = svdupq_n_u32 (-256, 0xff00, -256, 0xff00),
		z0 = svdupq_u32 (-256, 0xff00, -256, 0xff00))

/*
** dupq_fedcd_u32:
**	mov	(x[0-9]+), 65244
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dupq_fedcd_u32, svuint32_t,
		z0 = svdupq_n_u32 (0xfedc, 0, 0xfedc, 0),
		z0 = svdupq_u32 (0xfedc, 0, 0xfedc, 0))

/*
** dupq_1357ud_u32:
**	mov	(x[0-9]+), 21264383082496
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dupq_1357ud_u32, svuint32_t,
		z0 = svdupq_n_u32 (0, 0x1357, 0, 0x1357),
		z0 = svdupq_u32 (0, 0x1357, 0, 0x1357))

/*
** dupq_pool_u32:
**	...
**	ld1rqw	z0\.s, p[0-7]/z, \[x[0-9]+\]
**	ret
*/
TEST_UNIFORM_Z (dupq_pool_u32, svuint32_t,
		z0 = svdupq_n_u32 (4, 10, 9, 77),
		z0 = svdupq_u32 (4, 10, 9, 77))
