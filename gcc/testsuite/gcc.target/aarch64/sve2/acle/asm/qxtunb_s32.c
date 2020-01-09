/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qxtunb_s32:
**	sqxtunb	z0\.h, z4\.s
**	ret
*/
TEST_DUAL_Z (qxtunb_s32, svuint16_t, svint32_t,
	     z0 = svqxtunb_s32 (z4),
	     z0 = svqxtunb (z4))
