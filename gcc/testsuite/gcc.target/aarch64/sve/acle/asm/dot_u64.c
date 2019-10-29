/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dot_u64_tied1:
**	udot	z0\.d, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (dot_u64_tied1, svuint64_t, svuint16_t,
	     z0 = svdot_u64 (z0, z4, z5),
	     z0 = svdot (z0, z4, z5))

/*
** dot_u64_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	udot	z0\.d, \1\.h, z1\.h
**	ret
*/
TEST_DUAL_Z_REV (dot_u64_tied2, svuint64_t, svuint16_t,
		 z0_res = svdot_u64 (z4, z0, z1),
		 z0_res = svdot (z4, z0, z1))

/*
** dot_u64_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	udot	z0\.d, z1\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (dot_u64_tied3, svuint64_t, svuint16_t,
		 z0_res = svdot_u64 (z4, z1, z0),
		 z0_res = svdot (z4, z1, z0))

/*
** dot_u64_untied:
**	movprfx	z0, z1
**	udot	z0\.d, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (dot_u64_untied, svuint64_t, svuint16_t,
	     z0 = svdot_u64 (z1, z4, z5),
	     z0 = svdot (z1, z4, z5))

/*
** dot_w0_u64_tied1:
**	mov	(z[0-9]+\.h), w0
**	udot	z0\.d, z4\.h, \1
**	ret
*/
TEST_DUAL_ZX (dot_w0_u64_tied1, svuint64_t, svuint16_t, uint16_t,
	      z0 = svdot_n_u64 (z0, z4, x0),
	      z0 = svdot (z0, z4, x0))

/*
** dot_w0_u64_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	udot	z0\.d, z4\.h, \1
**	ret
*/
TEST_DUAL_ZX (dot_w0_u64_untied, svuint64_t, svuint16_t, uint16_t,
	      z0 = svdot_n_u64 (z1, z4, x0),
	      z0 = svdot (z1, z4, x0))

/*
** dot_9_u64_tied1:
**	mov	(z[0-9]+\.h), #9
**	udot	z0\.d, z4\.h, \1
**	ret
*/
TEST_DUAL_Z (dot_9_u64_tied1, svuint64_t, svuint16_t,
	     z0 = svdot_n_u64 (z0, z4, 9),
	     z0 = svdot (z0, z4, 9))

/*
** dot_9_u64_untied: { xfail *-*-* }
**	mov	(z[0-9]+\.h), #9
**	movprfx	z0, z1
**	udot	z0\.d, z4\.h, \1
**	ret
*/
TEST_DUAL_Z (dot_9_u64_untied, svuint64_t, svuint16_t,
	     z0 = svdot_n_u64 (z1, z4, 9),
	     z0 = svdot (z1, z4, 9))
