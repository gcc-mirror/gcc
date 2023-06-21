/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** clastb_f64_tied1:
**	clastb	z0\.d, p0, z0\.d, z1\.d
**	ret
*/
TEST_UNIFORM_Z (clastb_f64_tied1, svfloat64_t,
		z0 = svclastb_f64 (p0, z0, z1),
		z0 = svclastb (p0, z0, z1))

/*
** clastb_f64_tied2:
**	mov	(z[0-9]+\.d), z0\.d
**	movprfx	z0, z1
**	clastb	z0\.d, p0, z0\.d, \1
**	ret
*/
TEST_UNIFORM_Z (clastb_f64_tied2, svfloat64_t,
		z0 = svclastb_f64 (p0, z1, z0),
		z0 = svclastb (p0, z1, z0))

/*
** clastb_f64_untied:
**	movprfx	z0, z1
**	clastb	z0\.d, p0, z0\.d, z2\.d
**	ret
*/
TEST_UNIFORM_Z (clastb_f64_untied, svfloat64_t,
		z0 = svclastb_f64 (p0, z1, z2),
		z0 = svclastb (p0, z1, z2))

/*
** clastb_d0_f64:
**	clastb	d0, p0, d0, z2\.d
**	ret
*/
TEST_FOLD_LEFT_D (clastb_d0_f64, float64_t, svfloat64_t,
		  d0 = svclastb_n_f64 (p0, d0, z2),
		  d0 = svclastb (p0, d0, z2))

/*
** clastb_d1_f64:
** (
**	fmov	d0, d1
**	clastb	d0, p0, d0, z2\.d
** |
**	clastb	d1, p0, d1, z2\.d
**	fmov	d0, d1
** )
**	ret
*/
TEST_FOLD_LEFT_D (clastb_d1_f64, float64_t, svfloat64_t,
		  d0 = svclastb_n_f64 (p0, d1, z2),
		  d0 = svclastb (p0, d1, z2))
