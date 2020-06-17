/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** tbx_s8_tied1:
**	tbx	z0\.b, z1\.b, z4\.b
**	ret
*/
TEST_DUAL_Z (tbx_s8_tied1, svint8_t, svuint8_t,
	     z0 = svtbx_s8 (z0, z1, z4),
	     z0 = svtbx (z0, z1, z4))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z (tbx_s8_tied2, svint8_t, svuint8_t,
	     z0 = svtbx_s8 (z1, z0, z4),
	     z0 = svtbx (z1, z0, z4))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (tbx_s8_tied3, svint8_t, svuint8_t,
		 z0_res = svtbx_s8 (z4, z5, z0),
		 z0_res = svtbx (z4, z5, z0))

/*
** tbx_s8_untied:
** (
**	mov	z0\.d, z1\.d
**	tbx	z0\.b, z2\.b, z4\.b
** |
**	tbx	z1\.b, z2\.b, z4\.b
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (tbx_s8_untied, svint8_t, svuint8_t,
	     z0 = svtbx_s8 (z1, z2, z4),
	     z0 = svtbx (z1, z2, z4))
