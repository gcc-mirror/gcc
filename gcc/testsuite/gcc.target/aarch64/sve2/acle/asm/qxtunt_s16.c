/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qxtunt_s16_tied1:
**	sqxtunt	z0\.b, z4\.h
**	ret
*/
TEST_DUAL_Z (qxtunt_s16_tied1, svuint8_t, svint16_t,
	     z0 = svqxtunt_s16 (z0, z4),
	     z0 = svqxtunt (z0, z4))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (qxtunt_s16_tied2, svuint8_t, svint16_t,
		 z0_res = svqxtunt_s16 (z4, z0),
		 z0_res = svqxtunt (z4, z0))

/*
** qxtunt_s16_untied:
** (
**	mov	z0\.d, z1\.d
**	sqxtunt	z0\.b, z4\.h
** |
**	sqxtunt	z1\.b, z4\.h
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qxtunt_s16_untied, svuint8_t, svint16_t,
	     z0 = svqxtunt_s16 (z1, z4),
	     z0 = svqxtunt (z1, z4))
