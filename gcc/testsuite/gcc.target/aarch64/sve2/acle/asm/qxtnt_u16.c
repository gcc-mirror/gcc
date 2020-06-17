/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qxtnt_u16_tied1:
**	uqxtnt	z0\.b, z4\.h
**	ret
*/
TEST_DUAL_Z (qxtnt_u16_tied1, svuint8_t, svuint16_t,
	     z0 = svqxtnt_u16 (z0, z4),
	     z0 = svqxtnt (z0, z4))

/* Bad RA choice: no preferred output sequence.  */
TEST_DUAL_Z_REV (qxtnt_u16_tied2, svuint8_t, svuint16_t,
		 z0_res = svqxtnt_u16 (z4, z0),
		 z0_res = svqxtnt (z4, z0))

/*
** qxtnt_u16_untied:
** (
**	mov	z0\.d, z1\.d
**	uqxtnt	z0\.b, z4\.h
** |
**	uqxtnt	z1\.b, z4\.h
**	mov	z0\.d, z1\.d
** )
**	ret
*/
TEST_DUAL_Z (qxtnt_u16_untied, svuint8_t, svuint16_t,
	     z0 = svqxtnt_u16 (z1, z4),
	     z0 = svqxtnt (z1, z4))
