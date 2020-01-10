/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qxtunt_s64_tied1:
**	sqxtunt	z0\.s, z4\.d
**	ret
*/
TEST_DUAL_Z (qxtunt_s64_tied1, svuint32_t, svint64_t,
	     z0 = svqxtunt_s64 (z0, z4),
	     z0 = svqxtunt (z0, z4))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (qxtunt_s64_tied2, svuint32_t, svint64_t,
		 z0_res = svqxtunt_s64 (z4, z0),
		 z0_res = svqxtunt (z4, z0))

/*
** qxtunt_s64_untied:
** (
**	mov	z0\.d, z1\.d
**	sqxtunt	z0\.s, z4\.d
** |
**	sqxtunt	z1\.s, z4\.d
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qxtunt_s64_untied, svuint32_t, svint64_t,
	     z0 = svqxtunt_s64 (z1, z4),
	     z0 = svqxtunt (z1, z4))
