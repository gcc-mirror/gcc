/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qdecp_u32_tied:
**	uqdecp	z0\.s, p0
**	ret
*/
TEST_UNIFORM_Z (qdecp_u32_tied, svuint32_t,
		z0 = svqdecp_u32 (z0, p0),
		z0 = svqdecp (z0, p0))

/*
** qdecp_u32_untied:
**	movprfx	z0, z1
**	uqdecp	z0\.s, p0
**	ret
*/
TEST_UNIFORM_Z (qdecp_u32_untied, svuint32_t,
		z0 = svqdecp_u32 (z1, p0),
		z0 = svqdecp (z1, p0))

/*
** qdecp_n_u32_b8_tied:
**	uqdecp	w0, p0\.b
**	ret
*/
TEST_UNIFORM_S (qdecp_n_u32_b8_tied, uint32_t,
		x0 = svqdecp_n_u32_b8 (x0, p0),
		x0 = svqdecp_b8 (x0, p0))

/*
** qdecp_n_u32_b8_untied:
**	mov	w0, w1
**	uqdecp	w0, p0\.b
**	ret
*/
TEST_UNIFORM_S (qdecp_n_u32_b8_untied, uint32_t,
		x0 = svqdecp_n_u32_b8 (x1, p0),
		x0 = svqdecp_b8 (x1, p0))

/*
** qdecp_n_u32_b16_tied:
**	uqdecp	w0, p0\.h
**	ret
*/
TEST_UNIFORM_S (qdecp_n_u32_b16_tied, uint32_t,
		x0 = svqdecp_n_u32_b16 (x0, p0),
		x0 = svqdecp_b16 (x0, p0))

/*
** qdecp_n_u32_b16_untied:
**	mov	w0, w1
**	uqdecp	w0, p0\.h
**	ret
*/
TEST_UNIFORM_S (qdecp_n_u32_b16_untied, uint32_t,
		x0 = svqdecp_n_u32_b16 (x1, p0),
		x0 = svqdecp_b16 (x1, p0))

/*
** qdecp_n_u32_b32_tied:
**	uqdecp	w0, p0\.s
**	ret
*/
TEST_UNIFORM_S (qdecp_n_u32_b32_tied, uint32_t,
		x0 = svqdecp_n_u32_b32 (x0, p0),
		x0 = svqdecp_b32 (x0, p0))

/*
** qdecp_n_u32_b32_untied:
**	mov	w0, w1
**	uqdecp	w0, p0\.s
**	ret
*/
TEST_UNIFORM_S (qdecp_n_u32_b32_untied, uint32_t,
		x0 = svqdecp_n_u32_b32 (x1, p0),
		x0 = svqdecp_b32 (x1, p0))

/*
** qdecp_n_u32_b64_tied:
**	uqdecp	w0, p0\.d
**	ret
*/
TEST_UNIFORM_S (qdecp_n_u32_b64_tied, uint32_t,
		x0 = svqdecp_n_u32_b64 (x0, p0),
		x0 = svqdecp_b64 (x0, p0))

/*
** qdecp_n_u32_b64_untied:
**	mov	w0, w1
**	uqdecp	w0, p0\.d
**	ret
*/
TEST_UNIFORM_S (qdecp_n_u32_b64_untied, uint32_t,
		x0 = svqdecp_n_u32_b64 (x1, p0),
		x0 = svqdecp_b64 (x1, p0))
