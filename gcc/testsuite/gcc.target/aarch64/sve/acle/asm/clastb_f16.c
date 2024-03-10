/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** clastb_f16_tied1:
**	clastb	z0\.h, p0, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (clastb_f16_tied1, svfloat16_t,
		z0 = svclastb_f16 (p0, z0, z1),
		z0 = svclastb (p0, z0, z1))

/*
** clastb_f16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	clastb	z0\.h, p0, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (clastb_f16_tied2, svfloat16_t,
		z0 = svclastb_f16 (p0, z1, z0),
		z0 = svclastb (p0, z1, z0))

/*
** clastb_f16_untied:
**	movprfx	z0, z1
**	clastb	z0\.h, p0, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (clastb_f16_untied, svfloat16_t,
		z0 = svclastb_f16 (p0, z1, z2),
		z0 = svclastb (p0, z1, z2))

/*
** clastb_d0_f16:
**	clastb	h0, p0, h0, z2\.h
**	ret
*/
TEST_FOLD_LEFT_D (clastb_d0_f16, float16_t, svfloat16_t,
		  d0 = svclastb_n_f16 (p0, d0, z2),
		  d0 = svclastb (p0, d0, z2))

/*
** clastb_d1_f16:
** (
**	mov	v0\.h\[0\], v1\.h\[0\]
**	clastb	h0, p0, h0, z2\.h
** |
**	clastb	h1, p0, h1, z2\.h
**	mov	v0\.h\[0\], v1\.h\[0\]
** )
**	ret
*/
TEST_FOLD_LEFT_D (clastb_d1_f16, float16_t, svfloat16_t,
		  d0 = svclastb_n_f16 (p0, d1, z2),
		  d0 = svclastb (p0, d1, z2))
