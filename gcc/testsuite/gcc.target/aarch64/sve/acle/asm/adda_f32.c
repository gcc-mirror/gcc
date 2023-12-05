/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** adda_d0_f32:
**	fadda	s0, p0, s0, z2\.s
**	ret
*/
TEST_FOLD_LEFT_D (adda_d0_f32, float32_t, svfloat32_t,
		  d0 = svadda_f32 (p0, d0, z2),
		  d0 = svadda (p0, d0, z2))

/*
** adda_d1_f32:
** (
**	fmov	s0, s1
**	fadda	s0, p0, s0, z2\.s
** |
**	fadda	s1, p0, s1, z2\.s
**	fmov	s0, s1
** )
**	ret
*/
TEST_FOLD_LEFT_D (adda_d1_f32, float32_t, svfloat32_t,
		  d0 = svadda_f32 (p0, d1, z2),
		  d0 = svadda (p0, d1, z2))
