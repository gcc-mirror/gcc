/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qincp_u64_tied:
**	uqincp	z0\.d, p0
**	ret
*/
TEST_UNIFORM_Z (qincp_u64_tied, svuint64_t,
		z0 = svqincp_u64 (z0, p0),
		z0 = svqincp (z0, p0))

/*
** qincp_u64_untied:
**	movprfx	z0, z1
**	uqincp	z0\.d, p0
**	ret
*/
TEST_UNIFORM_Z (qincp_u64_untied, svuint64_t,
		z0 = svqincp_u64 (z1, p0),
		z0 = svqincp (z1, p0))

/*
** qincp_n_u64_b8_tied:
**	uqincp	x0, p0\.b
**	ret
*/
TEST_UNIFORM_S (qincp_n_u64_b8_tied, uint64_t,
		x0 = svqincp_n_u64_b8 (x0, p0),
		x0 = svqincp_b8 (x0, p0))

/*
** qincp_n_u64_b8_untied:
**	mov	x0, x1
**	uqincp	x0, p0\.b
**	ret
*/
TEST_UNIFORM_S (qincp_n_u64_b8_untied, uint64_t,
		x0 = svqincp_n_u64_b8 (x1, p0),
		x0 = svqincp_b8 (x1, p0))

/*
** qincp_n_u64_b16_tied:
**	uqincp	x0, p0\.h
**	ret
*/
TEST_UNIFORM_S (qincp_n_u64_b16_tied, uint64_t,
		x0 = svqincp_n_u64_b16 (x0, p0),
		x0 = svqincp_b16 (x0, p0))

/*
** qincp_n_u64_b16_untied:
**	mov	x0, x1
**	uqincp	x0, p0\.h
**	ret
*/
TEST_UNIFORM_S (qincp_n_u64_b16_untied, uint64_t,
		x0 = svqincp_n_u64_b16 (x1, p0),
		x0 = svqincp_b16 (x1, p0))

/*
** qincp_n_u64_b32_tied:
**	uqincp	x0, p0\.s
**	ret
*/
TEST_UNIFORM_S (qincp_n_u64_b32_tied, uint64_t,
		x0 = svqincp_n_u64_b32 (x0, p0),
		x0 = svqincp_b32 (x0, p0))

/*
** qincp_n_u64_b32_untied:
**	mov	x0, x1
**	uqincp	x0, p0\.s
**	ret
*/
TEST_UNIFORM_S (qincp_n_u64_b32_untied, uint64_t,
		x0 = svqincp_n_u64_b32 (x1, p0),
		x0 = svqincp_b32 (x1, p0))

/*
** qincp_n_u64_b64_tied:
**	uqincp	x0, p0\.d
**	ret
*/
TEST_UNIFORM_S (qincp_n_u64_b64_tied, uint64_t,
		x0 = svqincp_n_u64_b64 (x0, p0),
		x0 = svqincp_b64 (x0, p0))

/*
** qincp_n_u64_b64_untied:
**	mov	x0, x1
**	uqincp	x0, p0\.d
**	ret
*/
TEST_UNIFORM_S (qincp_n_u64_b64_untied, uint64_t,
		x0 = svqincp_n_u64_b64 (x1, p0),
		x0 = svqincp_b64 (x1, p0))
