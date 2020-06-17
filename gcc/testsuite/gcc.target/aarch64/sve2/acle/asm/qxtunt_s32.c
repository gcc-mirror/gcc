/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qxtunt_s32_tied1:
**	sqxtunt	z0\.h, z4\.s
**	ret
*/
TEST_DUAL_Z (qxtunt_s32_tied1, svuint16_t, svint32_t,
	     z0 = svqxtunt_s32 (z0, z4),
	     z0 = svqxtunt (z0, z4))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (qxtunt_s32_tied2, svuint16_t, svint32_t,
		 z0_res = svqxtunt_s32 (z4, z0),
		 z0_res = svqxtunt (z4, z0))

/*
** qxtunt_s32_untied:
** (
**	mov	z0\.d, z1\.d
**	sqxtunt	z0\.h, z4\.s
** |
**	sqxtunt	z1\.h, z4\.s
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qxtunt_s32_untied, svuint16_t, svint32_t,
	     z0 = svqxtunt_s32 (z1, z4),
	     z0 = svqxtunt (z1, z4))
