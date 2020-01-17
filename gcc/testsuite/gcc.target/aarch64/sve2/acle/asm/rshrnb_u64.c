/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** rshrnb_1_u64:
**	rshrnb	z0\.s, z0\.d, #1
**	ret
*/
TEST_TYPE_CHANGE_Z (rshrnb_1_u64, svuint32_t, svuint64_t,
		    z0_res = svrshrnb_n_u64 (z0, 1),
		    z0_res = svrshrnb (z0, 1))

/*
** rshrnb_2_u64:
**	rshrnb	z0\.s, z0\.d, #2
**	ret
*/
TEST_TYPE_CHANGE_Z (rshrnb_2_u64, svuint32_t, svuint64_t,
		    z0_res = svrshrnb_n_u64 (z0, 2),
		    z0_res = svrshrnb (z0, 2))

/*
** rshrnb_32_u64_tied1:
**	rshrnb	z0\.s, z0\.d, #32
**	ret
*/
TEST_TYPE_CHANGE_Z (rshrnb_32_u64_tied1, svuint32_t, svuint64_t,
		    z0_res = svrshrnb_n_u64 (z0, 32),
		    z0_res = svrshrnb (z0, 32))

/*
** rshrnb_32_u64_untied:
**	rshrnb	z0\.s, z1\.d, #32
**	ret
*/
TEST_TYPE_CHANGE_Z (rshrnb_32_u64_untied, svuint32_t, svuint64_t,
		    z0_res = svrshrnb_n_u64 (z1, 32),
		    z0_res = svrshrnb (z1, 32))
