/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qincp_s32_tied:
**	sqincp	z0\.s, p0
**	ret
*/
TEST_UNIFORM_Z (qincp_s32_tied, svint32_t,
		z0 = svqincp_s32 (z0, p0),
		z0 = svqincp (z0, p0))

/*
** qincp_s32_untied:
**	movprfx	z0, z1
**	sqincp	z0\.s, p0
**	ret
*/
TEST_UNIFORM_Z (qincp_s32_untied, svint32_t,
		z0 = svqincp_s32 (z1, p0),
		z0 = svqincp (z1, p0))

/*
** qincp_n_s32_b8_tied:
**	sqincp	x0, p0\.b, w0
**	ret
*/
TEST_UNIFORM_S (qincp_n_s32_b8_tied, int32_t,
		x0 = svqincp_n_s32_b8 (x0, p0),
		x0 = svqincp_b8 (x0, p0))

/*
** qincp_n_s32_b8_untied:
**	mov	w0, w1
**	sqincp	x0, p0\.b, w0
**	ret
*/
TEST_UNIFORM_S (qincp_n_s32_b8_untied, int32_t,
		x0 = svqincp_n_s32_b8 (x1, p0),
		x0 = svqincp_b8 (x1, p0))

/*
** qincp_n_s32_b16_tied:
**	sqincp	x0, p0\.h, w0
**	ret
*/
TEST_UNIFORM_S (qincp_n_s32_b16_tied, int32_t,
		x0 = svqincp_n_s32_b16 (x0, p0),
		x0 = svqincp_b16 (x0, p0))

/*
** qincp_n_s32_b16_untied:
**	mov	w0, w1
**	sqincp	x0, p0\.h, w0
**	ret
*/
TEST_UNIFORM_S (qincp_n_s32_b16_untied, int32_t,
		x0 = svqincp_n_s32_b16 (x1, p0),
		x0 = svqincp_b16 (x1, p0))

/*
** qincp_n_s32_b32_tied:
**	sqincp	x0, p0\.s, w0
**	ret
*/
TEST_UNIFORM_S (qincp_n_s32_b32_tied, int32_t,
		x0 = svqincp_n_s32_b32 (x0, p0),
		x0 = svqincp_b32 (x0, p0))

/*
** qincp_n_s32_b32_untied:
**	mov	w0, w1
**	sqincp	x0, p0\.s, w0
**	ret
*/
TEST_UNIFORM_S (qincp_n_s32_b32_untied, int32_t,
		x0 = svqincp_n_s32_b32 (x1, p0),
		x0 = svqincp_b32 (x1, p0))

/*
** qincp_n_s32_b64_tied:
**	sqincp	x0, p0\.d, w0
**	ret
*/
TEST_UNIFORM_S (qincp_n_s32_b64_tied, int32_t,
		x0 = svqincp_n_s32_b64 (x0, p0),
		x0 = svqincp_b64 (x0, p0))

/*
** qincp_n_s32_b64_untied:
**	mov	w0, w1
**	sqincp	x0, p0\.d, w0
**	ret
*/
TEST_UNIFORM_S (qincp_n_s32_b64_untied, int32_t,
		x0 = svqincp_n_s32_b64 (x1, p0),
		x0 = svqincp_b64 (x1, p0))
