/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdecp_s32_tied:
**	sqdecp	z0\.s, p0
**	ret
*/
TEST_UNIFORM_Z (qdecp_s32_tied, svint32_t,
		z0 = svqdecp_s32 (z0, p0),
		z0 = svqdecp (z0, p0))

/*
** qdecp_s32_untied:
**	movprfx	z0, z1
**	sqdecp	z0\.s, p0
**	ret
*/
TEST_UNIFORM_Z (qdecp_s32_untied, svint32_t,
		z0 = svqdecp_s32 (z1, p0),
		z0 = svqdecp (z1, p0))

/*
** qdecp_n_s32_b8_tied:
**	sqdecp	x0, p0\.b, w0
**	ret
*/
TEST_UNIFORM_S (qdecp_n_s32_b8_tied, int32_t,
		x0 = svqdecp_n_s32_b8 (x0, p0),
		x0 = svqdecp_b8 (x0, p0))

/*
** qdecp_n_s32_b8_untied:
**	mov	w0, w1
**	sqdecp	x0, p0\.b, w0
**	ret
*/
TEST_UNIFORM_S (qdecp_n_s32_b8_untied, int32_t,
		x0 = svqdecp_n_s32_b8 (x1, p0),
		x0 = svqdecp_b8 (x1, p0))

/*
** qdecp_n_s32_b16_tied:
**	sqdecp	x0, p0\.h, w0
**	ret
*/
TEST_UNIFORM_S (qdecp_n_s32_b16_tied, int32_t,
		x0 = svqdecp_n_s32_b16 (x0, p0),
		x0 = svqdecp_b16 (x0, p0))

/*
** qdecp_n_s32_b16_untied:
**	mov	w0, w1
**	sqdecp	x0, p0\.h, w0
**	ret
*/
TEST_UNIFORM_S (qdecp_n_s32_b16_untied, int32_t,
		x0 = svqdecp_n_s32_b16 (x1, p0),
		x0 = svqdecp_b16 (x1, p0))

/*
** qdecp_n_s32_b32_tied:
**	sqdecp	x0, p0\.s, w0
**	ret
*/
TEST_UNIFORM_S (qdecp_n_s32_b32_tied, int32_t,
		x0 = svqdecp_n_s32_b32 (x0, p0),
		x0 = svqdecp_b32 (x0, p0))

/*
** qdecp_n_s32_b32_untied:
**	mov	w0, w1
**	sqdecp	x0, p0\.s, w0
**	ret
*/
TEST_UNIFORM_S (qdecp_n_s32_b32_untied, int32_t,
		x0 = svqdecp_n_s32_b32 (x1, p0),
		x0 = svqdecp_b32 (x1, p0))

/*
** qdecp_n_s32_b64_tied:
**	sqdecp	x0, p0\.d, w0
**	ret
*/
TEST_UNIFORM_S (qdecp_n_s32_b64_tied, int32_t,
		x0 = svqdecp_n_s32_b64 (x0, p0),
		x0 = svqdecp_b64 (x0, p0))

/*
** qdecp_n_s32_b64_untied:
**	mov	w0, w1
**	sqdecp	x0, p0\.d, w0
**	ret
*/
TEST_UNIFORM_S (qdecp_n_s32_b64_untied, int32_t,
		x0 = svqdecp_n_s32_b64 (x1, p0),
		x0 = svqdecp_b64 (x1, p0))
