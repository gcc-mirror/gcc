/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** histseg_u8_tied1:
**	histseg	z0\.b, z0\.b, z1\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (histseg_u8_tied1, svuint8_t, svuint8_t,
		    z0_res = svhistseg_u8 (z0, z1),
		    z0_res = svhistseg (z0, z1))

/*
** histseg_u8_tied2:
**	histseg	z0\.b, z1\.b, z0\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (histseg_u8_tied2, svuint8_t, svuint8_t,
		    z0_res = svhistseg_u8 (z1, z0),
		    z0_res = svhistseg (z1, z0))

/*
** histseg_u8_untied:
**	histseg	z0\.b, z1\.b, z2\.b
**	ret
*/
TEST_TYPE_CHANGE_Z (histseg_u8_untied, svuint8_t, svuint8_t,
		    z0_res = svhistseg_u8 (z1, z2),
		    z0_res = svhistseg (z1, z2))
