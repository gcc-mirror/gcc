/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdecp_u64_tied:
**	uqdecp	z0\.d, p0
**	ret
*/
TEST_UNIFORM_Z (qdecp_u64_tied, svuint64_t,
		z0 = svqdecp_u64 (z0, p0),
		z0 = svqdecp (z0, p0))

/*
** qdecp_u64_untied:
**	movprfx	z0, z1
**	uqdecp	z0\.d, p0
**	ret
*/
TEST_UNIFORM_Z (qdecp_u64_untied, svuint64_t,
		z0 = svqdecp_u64 (z1, p0),
		z0 = svqdecp (z1, p0))

/*
** qdecp_n_u64_b8_tied:
**	uqdecp	x0, p0\.b
**	ret
*/
TEST_UNIFORM_S (qdecp_n_u64_b8_tied, uint64_t,
		x0 = svqdecp_n_u64_b8 (x0, p0),
		x0 = svqdecp_b8 (x0, p0))

/*
** qdecp_n_u64_b8_untied:
**	mov	x0, x1
**	uqdecp	x0, p0\.b
**	ret
*/
TEST_UNIFORM_S (qdecp_n_u64_b8_untied, uint64_t,
		x0 = svqdecp_n_u64_b8 (x1, p0),
		x0 = svqdecp_b8 (x1, p0))

/*
** qdecp_n_u64_b16_tied:
**	uqdecp	x0, p0\.h
**	ret
*/
TEST_UNIFORM_S (qdecp_n_u64_b16_tied, uint64_t,
		x0 = svqdecp_n_u64_b16 (x0, p0),
		x0 = svqdecp_b16 (x0, p0))

/*
** qdecp_n_u64_b16_untied:
**	mov	x0, x1
**	uqdecp	x0, p0\.h
**	ret
*/
TEST_UNIFORM_S (qdecp_n_u64_b16_untied, uint64_t,
		x0 = svqdecp_n_u64_b16 (x1, p0),
		x0 = svqdecp_b16 (x1, p0))

/*
** qdecp_n_u64_b32_tied:
**	uqdecp	x0, p0\.s
**	ret
*/
TEST_UNIFORM_S (qdecp_n_u64_b32_tied, uint64_t,
		x0 = svqdecp_n_u64_b32 (x0, p0),
		x0 = svqdecp_b32 (x0, p0))

/*
** qdecp_n_u64_b32_untied:
**	mov	x0, x1
**	uqdecp	x0, p0\.s
**	ret
*/
TEST_UNIFORM_S (qdecp_n_u64_b32_untied, uint64_t,
		x0 = svqdecp_n_u64_b32 (x1, p0),
		x0 = svqdecp_b32 (x1, p0))

/*
** qdecp_n_u64_b64_tied:
**	uqdecp	x0, p0\.d
**	ret
*/
TEST_UNIFORM_S (qdecp_n_u64_b64_tied, uint64_t,
		x0 = svqdecp_n_u64_b64 (x0, p0),
		x0 = svqdecp_b64 (x0, p0))

/*
** qdecp_n_u64_b64_untied:
**	mov	x0, x1
**	uqdecp	x0, p0\.d
**	ret
*/
TEST_UNIFORM_S (qdecp_n_u64_b64_untied, uint64_t,
		x0 = svqdecp_n_u64_b64 (x1, p0),
		x0 = svqdecp_b64 (x1, p0))
