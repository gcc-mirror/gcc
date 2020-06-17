/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qxtnt_u64_tied1:
**	uqxtnt	z0\.s, z4\.d
**	ret
*/
TEST_DUAL_Z (qxtnt_u64_tied1, svuint32_t, svuint64_t,
	     z0 = svqxtnt_u64 (z0, z4),
	     z0 = svqxtnt (z0, z4))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (qxtnt_u64_tied2, svuint32_t, svuint64_t,
		 z0_res = svqxtnt_u64 (z4, z0),
		 z0_res = svqxtnt (z4, z0))

/*
** qxtnt_u64_untied:
** (
**	mov	z0\.d, z1\.d
**	uqxtnt	z0\.s, z4\.d
** |
**	uqxtnt	z1\.s, z4\.d
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qxtnt_u64_untied, svuint32_t, svuint64_t,
	     z0 = svqxtnt_u64 (z1, z4),
	     z0 = svqxtnt (z1, z4))
