/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dupq_25600s_u16:
**	mov	z0\.s, #25600
**	ret
*/
TEST_UNIFORM_Z (dupq_25600s_u16, svuint16_t,
		z0 = svdupq_n_u16 (25600, 0, 25600, 0, 25600, 0, 25600, 0),
		z0 = svdupq_u16 (25600, 0, 25600, 0, 25600, 0, 25600, 0))

/*
** dupq_7ff00s_u16:
**	mov	z0\.s, #524032
**	ret
*/
TEST_UNIFORM_Z (dupq_7ff00s_u16, svuint16_t,
		z0 = svdupq_n_u16 (0xff00, 7, 0xff00, 7, 0xff00, 7, 0xff00, 7),
		z0 = svdupq_u16 (0xff00, 7, 0xff00, 7, 0xff00, 7, 0xff00, 7))

/*
** dupq_65536d_u16:
**	mov	z0\.d, #65536
**	ret
*/
TEST_UNIFORM_Z (dupq_65536d_u16, svuint16_t,
		z0 = svdupq_n_u16 (0, 1, 0, 0, 0, 1, 0, 0),
		z0 = svdupq_u16 (0, 1, 0, 0, 0, 1, 0, 0))

/*
** dupq_m2d_u16:
**	mov	z0\.d, #-2
**	ret
*/
TEST_UNIFORM_Z (dupq_m2d_u16, svuint16_t,
		z0 = svdupq_n_u16 (-2, -1, -1, -1, -2, -1, -1, -1),
		z0 = svdupq_u16 (-2, -1, -1, -1, -2, -1, -1, -1))

/*
** dupq_4ddb_u16:
**	movi	v([0-9]+)\.2d, 0xff0000ffff00ff
**	dup	z0\.q, z\1\.q\[0\]
**	ret
*/
TEST_UNIFORM_Z (dupq_4ddb_u16, svuint16_t,
		z0 = svdupq_n_u16 (0xff, -1, 0, 0xff, 0xff, -1, 0, 0xff),
		z0 = svdupq_u16 (0xff, -1, 0, 0xff, 0xff, -1, 0, 0xff))


/*
** dupq_a093s_u16:
**	mov	(w[0-9]+), 41107
**	mov	z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (dupq_a093s_u16, svuint16_t,
		z0 = svdupq_n_u16 (0xa093, 0, 0xa093, 0, 0xa093, 0, 0xa093, 0),
		z0 = svdupq_u16 (0xa093, 0, 0xa093, 0, 0xa093, 0, 0xa093, 0));

/*
** dupq_pool_u16:
**	...
**	ld1rqh	z0\.h, p[0-7]/z, \[x[0-9]+\]
**	ret
*/
TEST_UNIFORM_Z (dupq_pool_u16, svuint16_t,
		z0 = svdupq_n_u16 (4, 10, 9, 77, 52, 22, 19, 50),
		z0 = svdupq_u16 (4, 10, 9, 77, 52, 22, 19, 50))
