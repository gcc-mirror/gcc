/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qxtnt_s32_tied1:
**	sqxtnt	z0\.h, z4\.s
**	ret
*/
TEST_DUAL_Z (qxtnt_s32_tied1, svint16_t, svint32_t,
	     z0 = svqxtnt_s32 (z0, z4),
	     z0 = svqxtnt (z0, z4))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (qxtnt_s32_tied2, svint16_t, svint32_t,
		 z0_res = svqxtnt_s32 (z4, z0),
		 z0_res = svqxtnt (z4, z0))

/*
** qxtnt_s32_untied:
** (
**	mov	z0\.d, z1\.d
**	sqxtnt	z0\.h, z4\.s
** |
**	sqxtnt	z1\.h, z4\.s
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qxtnt_s32_untied, svint16_t, svint32_t,
	     z0 = svqxtnt_s32 (z1, z4),
	     z0 = svqxtnt (z1, z4))
