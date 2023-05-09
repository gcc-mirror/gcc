/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dot_s64_tied1:
**	sdot	z0\.d, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (dot_s64_tied1, svint64_t, svint16_t,
	     z0 = svdot_s64 (z0, z4, z5),
	     z0 = svdot (z0, z4, z5))

/*
** dot_s64_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sdot	z0\.d, \1\.h, z1\.h
**	ret
*/
TEST_DUAL_Z_REV (dot_s64_tied2, svint64_t, svint16_t,
		 z0_res = svdot_s64 (z4, z0, z1),
		 z0_res = svdot (z4, z0, z1))

/*
** dot_s64_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z4
**	sdot	z0\.d, z1\.h, \1\.h
**	ret
*/
TEST_DUAL_Z_REV (dot_s64_tied3, svint64_t, svint16_t,
		 z0_res = svdot_s64 (z4, z1, z0),
		 z0_res = svdot (z4, z1, z0))

/*
** dot_s64_untied:
**	movprfx	z0, z1
**	sdot	z0\.d, z4\.h, z5\.h
**	ret
*/
TEST_DUAL_Z (dot_s64_untied, svint64_t, svint16_t,
	     z0 = svdot_s64 (z1, z4, z5),
	     z0 = svdot (z1, z4, z5))

/*
** dot_w0_s64_tied1:
**	mov	(z[0-9]+\.h), w0
**	sdot	z0\.d, z4\.h, \1
**	ret
*/
TEST_DUAL_ZX (dot_w0_s64_tied1, svint64_t, svint16_t, int16_t,
	      z0 = svdot_n_s64 (z0, z4, x0),
	      z0 = svdot (z0, z4, x0))

/*
** dot_w0_s64_untied:
**	mov	(z[0-9]+\.h), w0
**	movprfx	z0, z1
**	sdot	z0\.d, z4\.h, \1
**	ret
*/
TEST_DUAL_ZX (dot_w0_s64_untied, svint64_t, svint16_t, int16_t,
	      z0 = svdot_n_s64 (z1, z4, x0),
	      z0 = svdot (z1, z4, x0))

/*
** dot_9_s64_tied1:
**	mov	(z[0-9]+\.h), #9
**	sdot	z0\.d, z4\.h, \1
**	ret
*/
TEST_DUAL_Z (dot_9_s64_tied1, svint64_t, svint16_t,
	     z0 = svdot_n_s64 (z0, z4, 9),
	     z0 = svdot (z0, z4, 9))

/*
** dot_9_s64_untied:
**	mov	(z[0-9]+\.h), #9
**	movprfx	z0, z1
**	sdot	z0\.d, z4\.h, \1
**	ret
*/
TEST_DUAL_Z (dot_9_s64_untied, svint64_t, svint16_t,
	     z0 = svdot_n_s64 (z1, z4, 9),
	     z0 = svdot (z1, z4, 9))
