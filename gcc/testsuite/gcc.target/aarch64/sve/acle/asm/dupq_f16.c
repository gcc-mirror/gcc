/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dupq_1c_f16:
**	mov	z0\.s, #15360
**	ret
*/
TEST_UNIFORM_Z (dupq_1c_f16, svfloat16_t,
		z0 = svdupq_n_f16 (1.0, 0, 1.0, 0, 1.0, 0, 1.0, 0),
		z0 = svdupq_f16 (1.0, 0, 1.0, 0, 1.0, 0, 1.0, 0));

/*
** dupq_5ic_f16:
**	movi	v([0-9]+)\.4s, 0x45, lsl 24
**	dup	z0\.q, z\1\.q\[0\]
**	ret
*/
TEST_UNIFORM_Z (dupq_5ic_f16, svfloat16_t,
		z0 = svdupq_n_f16 (0, 5.0, 0, 5.0, 0, 5.0, 0, 5.0),
		z0 = svdupq_f16 (0, 5.0, 0, 5.0, 0, 5.0, 0, 5.0));


/*
** dupq_m1c_f16:
**	movi	v([0-9]+)\.4s, 0xbc, lsl 8
**	dup	z0\.q, z\1\.q\[0\]
**	ret
*/
TEST_UNIFORM_Z (dupq_m1c_f16, svfloat16_t,
		z0 = svdupq_n_f16 (-1.0, 0, -1.0, 0, -1.0, 0, -1.0, 0),
		z0 = svdupq_f16 (-1.0, 0, -1.0, 0, -1.0, 0, -1.0, 0));

/*
** dupq_40p5c_f16:
**	mov	(w[0-9]+), 20752
**	mov	z0\.s, \1
**	ret
*/
TEST_UNIFORM_Z (dupq_40p5c_f16, svfloat16_t,
		z0 = svdupq_n_f16 (40.5, 0, 40.5, 0, 40.5, 0, 40.5, 0),
		z0 = svdupq_f16 (40.5, 0, 40.5, 0, 40.5, 0, 40.5, 0));

/*
** dupq_pool_f16:
**	...
**	ld1rqh	z0\.h, p[0-7]/z, \[x[0-9]+\]
**	ret
*/
TEST_UNIFORM_Z (dupq_pool_f16, svfloat16_t,
		z0 = svdupq_n_f16 (4.75, 1.0, 9, 77, 5.25, 22, 19, 50),
		z0 = svdupq_f16 (4.75, 1.0, 9, 77, 5.25, 22, 19, 50))
