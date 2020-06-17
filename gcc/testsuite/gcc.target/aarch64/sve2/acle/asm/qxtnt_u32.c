/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qxtnt_u32_tied1:
**	uqxtnt	z0\.h, z4\.s
**	ret
*/
TEST_DUAL_Z (qxtnt_u32_tied1, svuint16_t, svuint32_t,
	     z0 = svqxtnt_u32 (z0, z4),
	     z0 = svqxtnt (z0, z4))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (qxtnt_u32_tied2, svuint16_t, svuint32_t,
		 z0_res = svqxtnt_u32 (z4, z0),
		 z0_res = svqxtnt (z4, z0))

/*
** qxtnt_u32_untied:
** (
**	mov	z0\.d, z1\.d
**	uqxtnt	z0\.h, z4\.s
** |
**	uqxtnt	z1\.h, z4\.s
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qxtnt_u32_untied, svuint16_t, svuint32_t,
	     z0 = svqxtnt_u32 (z1, z4),
	     z0 = svqxtnt (z1, z4))
