/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** clastb_u32_tied1:
**	clastb	z0\.s, p0, z0\.s, z1\.s
**	ret
*/
TEST_UNIFORM_Z (clastb_u32_tied1, svuint32_t,
		z0 = svclastb_u32 (p0, z0, z1),
		z0 = svclastb (p0, z0, z1))

/*
** clastb_u32_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	clastb	z0\.s, p0, z0\.s, \1\.s
**	ret
*/
TEST_UNIFORM_Z (clastb_u32_tied2, svuint32_t,
		z0 = svclastb_u32 (p0, z1, z0),
		z0 = svclastb (p0, z1, z0))

/*
** clastb_u32_untied:
**	movprfx	z0, z1
**	clastb	z0\.s, p0, z0\.s, z2\.s
**	ret
*/
TEST_UNIFORM_Z (clastb_u32_untied, svuint32_t,
		z0 = svclastb_u32 (p0, z1, z2),
		z0 = svclastb (p0, z1, z2))

/*
** clastb_x0_u32:
**	clastb	w0, p0, w0, z0\.s
**	ret
*/
TEST_FOLD_LEFT_X (clastb_x0_u32, uint32_t, svuint32_t,
		  x0 = svclastb_n_u32 (p0, x0, z0),
		  x0 = svclastb (p0, x0, z0))

/*
** clastb_x1_u32:
**	mov	w0, w1
**	clastb	w0, p0, w0, z0\.s
**	ret
*/
TEST_FOLD_LEFT_X (clastb_x1_u32, uint32_t, svuint32_t,
		  x0 = svclastb_n_u32 (p0, x1, z0),
		  x0 = svclastb (p0, x1, z0))
