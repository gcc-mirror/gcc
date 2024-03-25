/* { dg-skip-if "" { *-*-* } { "-DSTREAMING_COMPATIBLE" } { "" } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** adda_d0_f16:
**	fadda	h0, p0, h0, z2\.h
**	ret
*/
TEST_FOLD_LEFT_D (adda_d0_f16, float16_t, svfloat16_t,
		  d0 = svadda_f16 (p0, d0, z2),
		  d0 = svadda (p0, d0, z2))

/*
** adda_d1_f16:
** (
**	mov	v0\.h\[0\], v1\.h\[0\]
**	fadda	h0, p0, h0, z2\.h
** |
**	fadda	h1, p0, h1, z2\.h
**	mov	v0\.h\[0\], v1\.h\[0\]
** )
**	ret
*/
TEST_FOLD_LEFT_D (adda_d1_f16, float16_t, svfloat16_t,
		  d0 = svadda_f16 (p0, d1, z2),
		  d0 = svadda (p0, d1, z2))
