/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** create4_s8:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_CREATE (create4_s8, svint8x4_t, svint8_t,
	     z0 = svcreate4_s8 (z6, z4, z7, z5),
	     z0 = svcreate4 (z6, z4, z7, z5))

/*
** create4_u8:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_CREATE (create4_u8, svuint8x4_t, svuint8_t,
	     z0 = svcreate4_u8 (z4, z6, z5, z7),
	     z0 = svcreate4 (z4, z6, z5, z7))

/*
** create4_s16:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_CREATE (create4_s16, svint16x4_t, svint16_t,
	     z0 = svcreate4_s16 (z6, z4, z5, z7),
	     z0 = svcreate4 (z6, z4, z5, z7))

/*
** create4_u16:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_CREATE (create4_u16, svuint16x4_t, svuint16_t,
	     z0 = svcreate4_u16 (z6, z5, z4, z7),
	     z0 = svcreate4 (z6, z5, z4, z7))

/*
** create4_bf16:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_CREATE (create4_bf16, svbfloat16x4_t, svbfloat16_t,
	     z0 = svcreate4_bf16 (z4, z5, z6, z7),
	     z0 = svcreate4 (z4, z5, z6, z7))

/*
** create4_f16:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_CREATE (create4_f16, svfloat16x4_t, svfloat16_t,
	     z0 = svcreate4_f16 (z4, z5, z6, z7),
	     z0 = svcreate4 (z4, z5, z6, z7))

/*
** create4_s32:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_CREATE (create4_s32, svint32x4_t, svint32_t,
	     z0 = svcreate4_s32 (z6, z7, z4, z5),
	     z0 = svcreate4 (z6, z7, z4, z5))

/*
** create4_u32:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_CREATE (create4_u32, svuint32x4_t, svuint32_t,
	     z0 = svcreate4_u32 (z7, z5, z6, z7),
	     z0 = svcreate4 (z7, z5, z6, z7))

/*
** create4_f32:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_CREATE (create4_f32, svfloat32x4_t, svfloat32_t,
	     z0 = svcreate4_f32 (z7, z4, z6, z4),
	     z0 = svcreate4 (z7, z4, z6, z4))

/*
** create4_s64:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_CREATE (create4_s64, svint64x4_t, svint64_t,
	     z0 = svcreate4_s64 (z5, z7, z6, z6),
	     z0 = svcreate4 (z5, z7, z6, z6))

/*
** create4_u64:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_CREATE (create4_u64, svuint64x4_t, svuint64_t,
	     z0 = svcreate4_u64 (z7, z6, z4, z5),
	     z0 = svcreate4 (z7, z6, z4, z5))

/*
** create4_f64:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_CREATE (create4_f64, svfloat64x4_t, svfloat64_t,
	     z0 = svcreate4_f64 (z5, z4, z7, z6),
	     z0 = svcreate4 (z5, z4, z7, z6))

/* This is awkward to code-generate, so don't match a particular output.  */
TEST_CREATE_B (create4_b_0, svboolx4_t,
	       p0_res = svcreate4_b (p0, p1, p2, p3),
	       p0_res = svcreate4 (p0, p1, p2, p3))

/* This is awkward to code-generate, so don't match a particular output.  */
TEST_CREATE_B (create4_b_1, svboolx4_t,
	       p0_res = svcreate4_b (p3, p2, p1, p0),
	       p0_res = svcreate4 (p3, p2, p1, p0))
