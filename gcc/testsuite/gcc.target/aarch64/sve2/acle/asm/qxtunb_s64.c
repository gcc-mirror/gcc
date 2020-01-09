/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qxtunb_s64:
**	sqxtunb	z0\.s, z4\.d
**	ret
*/
TEST_DUAL_Z (qxtunb_s64, svuint32_t, svint64_t,
	     z0 = svqxtunb_s64 (z4),
	     z0 = svqxtunb (z4))
