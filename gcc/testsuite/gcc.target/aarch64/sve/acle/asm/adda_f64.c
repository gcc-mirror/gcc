/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** adda_d0_f64:
**	fadda	d0, p0, d0, z2\.d
**	ret
*/
TEST_FOLD_LEFT_D (adda_d0_f64, float64_t, svfloat64_t,
		  d0 = svadda_f64 (p0, d0, z2),
		  d0 = svadda (p0, d0, z2))

/*
** adda_d1_f64:
** (
**	fmov	d0, d1
**	fadda	d0, p0, d0, z2\.d
** |
**	fadda	d1, p0, d1, z2\.d
**	fmov	d0, d1
** )
**	ret
*/
TEST_FOLD_LEFT_D (adda_d1_f64, float64_t, svfloat64_t,
		  d0 = svadda_f64 (p0, d1, z2),
		  d0 = svadda (p0, d1, z2))
