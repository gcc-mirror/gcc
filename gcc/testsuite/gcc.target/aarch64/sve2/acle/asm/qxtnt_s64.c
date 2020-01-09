/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qxtnt_s64_tied1:
**	sqxtnt	z0\.s, z4\.d
**	ret
*/
TEST_DUAL_Z (qxtnt_s64_tied1, svint32_t, svint64_t,
	     z0 = svqxtnt_s64 (z0, z4),
	     z0 = svqxtnt (z0, z4))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (qxtnt_s64_tied2, svint32_t, svint64_t,
		 z0_res = svqxtnt_s64 (z4, z0),
		 z0_res = svqxtnt (z4, z0))

/*
** qxtnt_s64_untied:
** (
**	mov	z0\.d, z1\.d
**	sqxtnt	z0\.s, z4\.d
** |
**	sqxtnt	z1\.s, z4\.d
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qxtnt_s64_untied, svint32_t, svint64_t,
	     z0 = svqxtnt_s64 (z1, z4),
	     z0 = svqxtnt (z1, z4))
