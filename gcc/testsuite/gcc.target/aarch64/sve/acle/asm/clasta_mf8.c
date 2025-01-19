/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** clasta_mf8_tied1:
**	clasta	z0\.b, p0, z0\.b, z1\.b
**	ret
*/
TEST_UNIFORM_Z (clasta_mf8_tied1, svmfloat8_t,
		z0 = svclasta_mf8 (p0, z0, z1),
		z0 = svclasta (p0, z0, z1))

/*
** clasta_mf8_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	clasta	z0\.b, p0, z0\.b, \1\.b
**	ret
*/
TEST_UNIFORM_Z (clasta_mf8_tied2, svmfloat8_t,
		z0 = svclasta_mf8 (p0, z1, z0),
		z0 = svclasta (p0, z1, z0))

/*
** clasta_mf8_untied:
**	movprfx	z0, z1
**	clasta	z0\.b, p0, z0\.b, z2\.b
**	ret
*/
TEST_UNIFORM_Z (clasta_mf8_untied, svmfloat8_t,
		z0 = svclasta_mf8 (p0, z1, z2),
		z0 = svclasta (p0, z1, z2))

/*
** clasta_x0_mf8:
**	clasta	b0, p0, b0, z2\.b
**	ret
*/
TEST_FOLD_LEFT_X (clasta_x0_mf8, mfloat8_t, svmfloat8_t,
		  x0 = svclasta_n_mf8 (p0, x0, z0),
		  x0 = svclasta (p0, x0, z0))

/*
** clasta_x1_mf8:
**	clasta	b1, p0, b1, z2\.b
**	dup	b0, v1.b\[0\]
**	ret
*/
TEST_FOLD_LEFT_X (clasta_x1_mf8, mfloat8_t, svmfloat8_t,
		  x0 = svclasta_n_mf8 (p0, x1, z0),
		  x0 = svclasta (p0, x1, z0))
