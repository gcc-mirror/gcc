/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** create3_s8:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_CREATE (create3_s8, svint8x3_t, svint8_t,
	     z0 = svcreate3_s8 (z6, z4, z7),
	     z0 = svcreate3 (z6, z4, z7))

/*
** create3_u8:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_CREATE (create3_u8, svuint8x3_t, svuint8_t,
	     z0 = svcreate3_u8 (z4, z6, z5),
	     z0 = svcreate3 (z4, z6, z5))

/*
** create3_s16:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_CREATE (create3_s16, svint16x3_t, svint16_t,
	     z0 = svcreate3_s16 (z6, z4, z5),
	     z0 = svcreate3 (z6, z4, z5))

/*
** create3_u16:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_CREATE (create3_u16, svuint16x3_t, svuint16_t,
	     z0 = svcreate3_u16 (z6, z5, z4),
	     z0 = svcreate3 (z6, z5, z4))

/*
** create3_bf16:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_CREATE (create3_bf16, svbfloat16x3_t, svbfloat16_t,
	     z0 = svcreate3_bf16 (z4, z5, z6),
	     z0 = svcreate3 (z4, z5, z6))

/*
** create3_f16:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_CREATE (create3_f16, svfloat16x3_t, svfloat16_t,
	     z0 = svcreate3_f16 (z4, z5, z6),
	     z0 = svcreate3 (z4, z5, z6))

/*
** create3_s32:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_CREATE (create3_s32, svint32x3_t, svint32_t,
	     z0 = svcreate3_s32 (z6, z7, z4),
	     z0 = svcreate3 (z6, z7, z4))

/*
** create3_u32:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_CREATE (create3_u32, svuint32x3_t, svuint32_t,
	     z0 = svcreate3_u32 (z7, z5, z6),
	     z0 = svcreate3 (z7, z5, z6))

/*
** create3_f32:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_CREATE (create3_f32, svfloat32x3_t, svfloat32_t,
	     z0 = svcreate3_f32 (z7, z4, z6),
	     z0 = svcreate3 (z7, z4, z6))

/*
** create3_s64:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_CREATE (create3_s64, svint64x3_t, svint64_t,
	     z0 = svcreate3_s64 (z5, z7, z6),
	     z0 = svcreate3 (z5, z7, z6))

/*
** create3_u64:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_CREATE (create3_u64, svuint64x3_t, svuint64_t,
	     z0 = svcreate3_u64 (z7, z6, z4),
	     z0 = svcreate3 (z7, z6, z4))

/*
** create3_f64:
**	mov	[^\n]+
**	mov	[^\n]+
**	mov	[^\n]+
**	ret
*/
TEST_CREATE (create3_f64, svfloat64x3_t, svfloat64_t,
	     z0 = svcreate3_f64 (z5, z4, z7),
	     z0 = svcreate3 (z5, z4, z7))
