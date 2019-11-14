/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** clasta_u64_tied1:
**	clasta	z0\.d, p0, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (clasta_u64_tied1, svuint64_t,
		z0 = svclasta_u64 (p0, z0, z1),
		z0 = svclasta (p0, z0, z1))

/*
** clasta_u64_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	clasta	z0\.d, p0, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (clasta_u64_tied2, svuint64_t,
		z0 = svclasta_u64 (p0, z1, z0),
		z0 = svclasta (p0, z1, z0))

/*
** clasta_u64_untied:
**	movprfx	z0, z1
**	clasta	z0\.d, p0, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (clasta_u64_untied, svuint64_t,
		z0 = svclasta_u64 (p0, z1, z2),
		z0 = svclasta (p0, z1, z2))

/*
** clasta_x0_u64:
**	clasta	x0, p0, x0, z0\.d
**	ret
*/
TEST_FOLD_LEFT_X (clasta_x0_u64, uint64_t, svuint64_t,
		  x0 = svclasta_n_u64 (p0, x0, z0),
		  x0 = svclasta (p0, x0, z0))

/*
** clasta_x1_u64:
**	mov	x0, x1
**	clasta	x0, p0, x0, z0\.d
**	ret
*/
TEST_FOLD_LEFT_X (clasta_x1_u64, uint64_t, svuint64_t,
		  x0 = svclasta_n_u64 (p0, x1, z0),
		  x0 = svclasta (p0, x1, z0))
