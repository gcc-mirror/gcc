/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdecp_s64_tied:
**	sqdecp	z0\.d, p0
**	ret
*/
TEST_UNIFORM_Z (qdecp_s64_tied, svint64_t,
		z0 = svqdecp_s64 (z0, p0),
		z0 = svqdecp (z0, p0))

/*
** qdecp_s64_untied:
**	movprfx	z0, z1
**	sqdecp	z0\.d, p0
**	ret
*/
TEST_UNIFORM_Z (qdecp_s64_untied, svint64_t,
		z0 = svqdecp_s64 (z1, p0),
		z0 = svqdecp (z1, p0))

/*
** qdecp_n_s64_b8_tied:
**	sqdecp	x0, p0\.b
**	ret
*/
TEST_UNIFORM_S (qdecp_n_s64_b8_tied, int64_t,
		x0 = svqdecp_n_s64_b8 (x0, p0),
		x0 = svqdecp_b8 (x0, p0))

/*
** qdecp_n_s64_b8_untied:
**	mov	x0, x1
**	sqdecp	x0, p0\.b
**	ret
*/
TEST_UNIFORM_S (qdecp_n_s64_b8_untied, int64_t,
		x0 = svqdecp_n_s64_b8 (x1, p0),
		x0 = svqdecp_b8 (x1, p0))

/*
** qdecp_n_s64_b16_tied:
**	sqdecp	x0, p0\.h
**	ret
*/
TEST_UNIFORM_S (qdecp_n_s64_b16_tied, int64_t,
		x0 = svqdecp_n_s64_b16 (x0, p0),
		x0 = svqdecp_b16 (x0, p0))

/*
** qdecp_n_s64_b16_untied:
**	mov	x0, x1
**	sqdecp	x0, p0\.h
**	ret
*/
TEST_UNIFORM_S (qdecp_n_s64_b16_untied, int64_t,
		x0 = svqdecp_n_s64_b16 (x1, p0),
		x0 = svqdecp_b16 (x1, p0))

/*
** qdecp_n_s64_b32_tied:
**	sqdecp	x0, p0\.s
**	ret
*/
TEST_UNIFORM_S (qdecp_n_s64_b32_tied, int64_t,
		x0 = svqdecp_n_s64_b32 (x0, p0),
		x0 = svqdecp_b32 (x0, p0))

/*
** qdecp_n_s64_b32_untied:
**	mov	x0, x1
**	sqdecp	x0, p0\.s
**	ret
*/
TEST_UNIFORM_S (qdecp_n_s64_b32_untied, int64_t,
		x0 = svqdecp_n_s64_b32 (x1, p0),
		x0 = svqdecp_b32 (x1, p0))

/*
** qdecp_n_s64_b64_tied:
**	sqdecp	x0, p0\.d
**	ret
*/
TEST_UNIFORM_S (qdecp_n_s64_b64_tied, int64_t,
		x0 = svqdecp_n_s64_b64 (x0, p0),
		x0 = svqdecp_b64 (x0, p0))

/*
** qdecp_n_s64_b64_untied:
**	mov	x0, x1
**	sqdecp	x0, p0\.d
**	ret
*/
TEST_UNIFORM_S (qdecp_n_s64_b64_untied, int64_t,
		x0 = svqdecp_n_s64_b64 (x1, p0),
		x0 = svqdecp_b64 (x1, p0))
