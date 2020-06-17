/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qxtunb_s16:
**	sqxtunb	z0\.b, z4\.h
**	ret
*/
TEST_DUAL_Z (qxtunb_s16, svuint8_t, svint16_t,
	     z0 = svqxtunb_s16 (z4),
	     z0 = svqxtunb (z4))
