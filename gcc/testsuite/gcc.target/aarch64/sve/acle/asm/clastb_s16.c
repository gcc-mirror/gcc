/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** clastb_s16_tied1:
**	clastb	z0\.h, p0, z0\.h, z1\.h
**	ret
*/
TEST_UNIFORM_Z (clastb_s16_tied1, svint16_t,
		z0 = svclastb_s16 (p0, z0, z1),
		z0 = svclastb (p0, z0, z1))

/*
** clastb_s16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	clastb	z0\.h, p0, z0\.h, \1\.h
**	ret
*/
TEST_UNIFORM_Z (clastb_s16_tied2, svint16_t,
		z0 = svclastb_s16 (p0, z1, z0),
		z0 = svclastb (p0, z1, z0))

/*
** clastb_s16_untied:
**	movprfx	z0, z1
**	clastb	z0\.h, p0, z0\.h, z2\.h
**	ret
*/
TEST_UNIFORM_Z (clastb_s16_untied, svint16_t,
		z0 = svclastb_s16 (p0, z1, z2),
		z0 = svclastb (p0, z1, z2))

/*
** clastb_x0_s16:
**	clastb	w0, p0, w0, z0\.h
**	ret
*/
TEST_FOLD_LEFT_X (clastb_x0_s16, int16_t, svint16_t,
		  x0 = svclastb_n_s16 (p0, x0, z0),
		  x0 = svclastb (p0, x0, z0))

/*
** clastb_x1_s16:
**	mov	w0, w1
**	clastb	w0, p0, w0, z0\.h
**	ret
*/
TEST_FOLD_LEFT_X (clastb_x1_s16, int16_t, svint16_t,
		  x0 = svclastb_n_s16 (p0, x1, z0),
		  x0 = svclastb (p0, x1, z0))
