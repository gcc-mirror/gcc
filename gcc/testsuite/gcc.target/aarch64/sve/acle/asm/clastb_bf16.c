/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** clastb_bf16_tied1:
**	clastb	z0\.h, p0, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (clastb_bf16_tied1, svbfloat16_t,
		z0 = svclastb_bf16 (p0, z0, z1),
		z0 = svclastb (p0, z0, z1))

/*
** clastb_bf16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	clastb	z0\.h, p0, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (clastb_bf16_tied2, svbfloat16_t,
		z0 = svclastb_bf16 (p0, z1, z0),
		z0 = svclastb (p0, z1, z0))

/*
** clastb_bf16_untied:
**	movprfx	z0, z1
**	clastb	z0\.h, p0, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (clastb_bf16_untied, svbfloat16_t,
		z0 = svclastb_bf16 (p0, z1, z2),
		z0 = svclastb (p0, z1, z2))

/*
** clastb_d0_bf16:
**	clastb	h0, p0, h0, z2\.h
**	ret
*/
TEST_FOLD_LEFT_D (clastb_d0_bf16, bfloat16_t, svbfloat16_t,
		  d0 = svclastb_n_bf16 (p0, d0, z2),
		  d0 = svclastb (p0, d0, z2))

/*
** clastb_d1_bf16:
**	mov	v0\.h\[0\], v1\.h\[0\]
**	clastb	h0, p0, h0, z2\.h
**	ret
*/
TEST_FOLD_LEFT_D (clastb_d1_bf16, bfloat16_t, svbfloat16_t,
		  d0 = svclastb_n_bf16 (p0, d1, z2),
		  d0 = svclastb (p0, d1, z2))
