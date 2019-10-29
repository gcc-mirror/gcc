/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** qincp_u32_tied:
**	uqincp	z0\.s, p0
**	ret
*/
TEST_UNIFORM_Z (qincp_u32_tied, svuint32_t,
		z0 = svqincp_u32 (z0, p0),
		z0 = svqincp (z0, p0))

/*
** qincp_u32_untied:
**	movprfx	z0, z1
**	uqincp	z0\.s, p0
**	ret
*/
TEST_UNIFORM_Z (qincp_u32_untied, svuint32_t,
		z0 = svqincp_u32 (z1, p0),
		z0 = svqincp (z1, p0))

/*
** qincp_n_u32_b8_tied:
**	uqincp	w0, p0\.b
**	ret
*/
TEST_UNIFORM_S (qincp_n_u32_b8_tied, uint32_t,
		x0 = svqincp_n_u32_b8 (x0, p0),
		x0 = svqincp_b8 (x0, p0))

/*
** qincp_n_u32_b8_untied:
**	mov	w0, w1
**	uqincp	w0, p0\.b
**	ret
*/
TEST_UNIFORM_S (qincp_n_u32_b8_untied, uint32_t,
		x0 = svqincp_n_u32_b8 (x1, p0),
		x0 = svqincp_b8 (x1, p0))

/*
** qincp_n_u32_b16_tied:
**	uqincp	w0, p0\.h
**	ret
*/
TEST_UNIFORM_S (qincp_n_u32_b16_tied, uint32_t,
		x0 = svqincp_n_u32_b16 (x0, p0),
		x0 = svqincp_b16 (x0, p0))

/*
** qincp_n_u32_b16_untied:
**	mov	w0, w1
**	uqincp	w0, p0\.h
**	ret
*/
TEST_UNIFORM_S (qincp_n_u32_b16_untied, uint32_t,
		x0 = svqincp_n_u32_b16 (x1, p0),
		x0 = svqincp_b16 (x1, p0))

/*
** qincp_n_u32_b32_tied:
**	uqincp	w0, p0\.s
**	ret
*/
TEST_UNIFORM_S (qincp_n_u32_b32_tied, uint32_t,
		x0 = svqincp_n_u32_b32 (x0, p0),
		x0 = svqincp_b32 (x0, p0))

/*
** qincp_n_u32_b32_untied:
**	mov	w0, w1
**	uqincp	w0, p0\.s
**	ret
*/
TEST_UNIFORM_S (qincp_n_u32_b32_untied, uint32_t,
		x0 = svqincp_n_u32_b32 (x1, p0),
		x0 = svqincp_b32 (x1, p0))

/*
** qincp_n_u32_b64_tied:
**	uqincp	w0, p0\.d
**	ret
*/
TEST_UNIFORM_S (qincp_n_u32_b64_tied, uint32_t,
		x0 = svqincp_n_u32_b64 (x0, p0),
		x0 = svqincp_b64 (x0, p0))

/*
** qincp_n_u32_b64_untied:
**	mov	w0, w1
**	uqincp	w0, p0\.d
**	ret
*/
TEST_UNIFORM_S (qincp_n_u32_b64_untied, uint32_t,
		x0 = svqincp_n_u32_b64 (x1, p0),
		x0 = svqincp_b64 (x1, p0))
