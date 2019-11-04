/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dupq_1c_f32:
**	mov	z0\.d, #1065353216
**	ret
*/
TEST_UNIFORM_Z (dupq_1c_f32, svfloat32_t,
		z0 = svdupq_n_f32 (1.0, 0, 1.0, 0),
		z0 = svdupq_f32 (1.0, 0, 1.0, 0));

/*
** dupq_5ic_f32:
**	mov	(x[0-9]+), 4656722014701092864
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dupq_5ic_f32, svfloat32_t,
		z0 = svdupq_n_f32 (0, 5.0, 0, 5.0),
		z0 = svdupq_f32 (0, 5.0, 0, 5.0));


/*
** dupq_m1c_f32:
**	mov	(x[0-9]+), 3212836864
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dupq_m1c_f32, svfloat32_t,
		z0 = svdupq_n_f32 (-1.0, 0, -1.0, 0),
		z0 = svdupq_f32 (-1.0, 0, -1.0, 0));

/*
** dupq_40p5c_f32:
**	mov	(x[0-9]+), 1109524480
**	mov	z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (dupq_40p5c_f32, svfloat32_t,
		z0 = svdupq_n_f32 (40.5, 0, 40.5, 0),
		z0 = svdupq_f32 (40.5, 0, 40.5, 0));

/*
** dupq_pool_f32:
**	...
**	ld1rqw	z0\.s, p[0-7]/z, \[x[0-9]+\]
**	ret
*/
TEST_UNIFORM_Z (dupq_pool_f32, svfloat32_t,
		z0 = svdupq_n_f32 (4.5, 10.1, 7.3, 11.8),
		z0 = svdupq_f32 (4.5, 10.1, 7.3, 11.8))
