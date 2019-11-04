/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** clasta_u16_tied1:
**	clasta	z0\.h, p0, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (clasta_u16_tied1, svuint16_t,
		z0 = svclasta_u16 (p0, z0, z1),
		z0 = svclasta (p0, z0, z1))

/*
** clasta_u16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	clasta	z0\.h, p0, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (clasta_u16_tied2, svuint16_t,
		z0 = svclasta_u16 (p0, z1, z0),
		z0 = svclasta (p0, z1, z0))

/*
** clasta_u16_untied:
**	movprfx	z0, z1
**	clasta	z0\.h, p0, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (clasta_u16_untied, svuint16_t,
		z0 = svclasta_u16 (p0, z1, z2),
		z0 = svclasta (p0, z1, z2))

/*
** clasta_x0_u16:
**	clasta	w0, p0, w0, z0\.h
**	ret
*/
TEST_FOLD_LEFT_X (clasta_x0_u16, uint16_t, svuint16_t,
		  x0 = svclasta_n_u16 (p0, x0, z0),
		  x0 = svclasta (p0, x0, z0))

/*
** clasta_x1_u16:
**	mov	w0, w1
**	clasta	w0, p0, w0, z0\.h
**	ret
*/
TEST_FOLD_LEFT_X (clasta_x1_u16, uint16_t, svuint16_t,
		  x0 = svclasta_n_u16 (p0, x1, z0),
		  x0 = svclasta (p0, x1, z0))
