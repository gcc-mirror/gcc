/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qxtnt_s16_tied1:
**	sqxtnt	z0\.b, z4\.h
**	ret
*/
TEST_DUAL_Z (qxtnt_s16_tied1, svint8_t, svint16_t,
	     z0 = svqxtnt_s16 (z0, z4),
	     z0 = svqxtnt (z0, z4))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (qxtnt_s16_tied2, svint8_t, svint16_t,
		 z0_res = svqxtnt_s16 (z4, z0),
		 z0_res = svqxtnt (z4, z0))

/*
** qxtnt_s16_untied:
** (
**	mov	z0\.d, z1\.d
**	sqxtnt	z0\.b, z4\.h
** |
**	sqxtnt	z1\.b, z4\.h
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qxtnt_s16_untied, svint8_t, svint16_t,
	     z0 = svqxtnt_s16 (z1, z4),
	     z0 = svqxtnt (z1, z4))
