/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qincp_s64_tied:
**	sqincp	z0\.d, p0
**	ret
*/
TEST_UNIFORM_Z (qincp_s64_tied, svint64_t,
		z0 = svqincp_s64 (z0, p0),
		z0 = svqincp (z0, p0))

/*
** qincp_s64_untied:
**	movprfx	z0, z1
**	sqincp	z0\.d, p0
**	ret
*/
TEST_UNIFORM_Z (qincp_s64_untied, svint64_t,
		z0 = svqincp_s64 (z1, p0),
		z0 = svqincp (z1, p0))

/*
** qincp_n_s64_b8_tied:
**	sqincp	x0, p0\.b
**	ret
*/
TEST_UNIFORM_S (qincp_n_s64_b8_tied, int64_t,
		x0 = svqincp_n_s64_b8 (x0, p0),
		x0 = svqincp_b8 (x0, p0))

/*
** qincp_n_s64_b8_untied:
**	mov	x0, x1
**	sqincp	x0, p0\.b
**	ret
*/
TEST_UNIFORM_S (qincp_n_s64_b8_untied, int64_t,
		x0 = svqincp_n_s64_b8 (x1, p0),
		x0 = svqincp_b8 (x1, p0))

/*
** qincp_n_s64_b16_tied:
**	sqincp	x0, p0\.h
**	ret
*/
TEST_UNIFORM_S (qincp_n_s64_b16_tied, int64_t,
		x0 = svqincp_n_s64_b16 (x0, p0),
		x0 = svqincp_b16 (x0, p0))

/*
** qincp_n_s64_b16_untied:
**	mov	x0, x1
**	sqincp	x0, p0\.h
**	ret
*/
TEST_UNIFORM_S (qincp_n_s64_b16_untied, int64_t,
		x0 = svqincp_n_s64_b16 (x1, p0),
		x0 = svqincp_b16 (x1, p0))

/*
** qincp_n_s64_b32_tied:
**	sqincp	x0, p0\.s
**	ret
*/
TEST_UNIFORM_S (qincp_n_s64_b32_tied, int64_t,
		x0 = svqincp_n_s64_b32 (x0, p0),
		x0 = svqincp_b32 (x0, p0))

/*
** qincp_n_s64_b32_untied:
**	mov	x0, x1
**	sqincp	x0, p0\.s
**	ret
*/
TEST_UNIFORM_S (qincp_n_s64_b32_untied, int64_t,
		x0 = svqincp_n_s64_b32 (x1, p0),
		x0 = svqincp_b32 (x1, p0))

/*
** qincp_n_s64_b64_tied:
**	sqincp	x0, p0\.d
**	ret
*/
TEST_UNIFORM_S (qincp_n_s64_b64_tied, int64_t,
		x0 = svqincp_n_s64_b64 (x0, p0),
		x0 = svqincp_b64 (x0, p0))

/*
** qincp_n_s64_b64_untied:
**	mov	x0, x1
**	sqincp	x0, p0\.d
**	ret
*/
TEST_UNIFORM_S (qincp_n_s64_b64_untied, int64_t,
		x0 = svqincp_n_s64_b64 (x1, p0),
		x0 = svqincp_b64 (x1, p0))
