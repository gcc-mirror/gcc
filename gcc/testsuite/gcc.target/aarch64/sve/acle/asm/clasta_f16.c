/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** clasta_f16_tied1:
**	clasta	z0\.h, p0, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (clasta_f16_tied1, svfloat16_t,
		z0 = svclasta_f16 (p0, z0, z1),
		z0 = svclasta (p0, z0, z1))

/*
** clasta_f16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	clasta	z0\.h, p0, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (clasta_f16_tied2, svfloat16_t,
		z0 = svclasta_f16 (p0, z1, z0),
		z0 = svclasta (p0, z1, z0))

/*
** clasta_f16_untied:
**	movprfx	z0, z1
**	clasta	z0\.h, p0, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (clasta_f16_untied, svfloat16_t,
		z0 = svclasta_f16 (p0, z1, z2),
		z0 = svclasta (p0, z1, z2))

/*
** clasta_d0_f16:
**	clasta	h0, p0, h0, z2\.h
**	ret
*/
TEST_FOLD_LEFT_D (clasta_d0_f16, float16_t, svfloat16_t,
		  d0 = svclasta_n_f16 (p0, d0, z2),
		  d0 = svclasta (p0, d0, z2))

/*
** clasta_d1_f16:
**	mov	v0\.h\[0\], v1\.h\[0\]
**	clasta	h0, p0, h0, z2\.h
**	ret
*/
TEST_FOLD_LEFT_D (clasta_d1_f16, float16_t, svfloat16_t,
		  d0 = svclasta_n_f16 (p0, d1, z2),
		  d0 = svclasta (p0, d1, z2))
