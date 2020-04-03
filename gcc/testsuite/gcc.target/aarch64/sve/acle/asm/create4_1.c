/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** create4_s8:
**	mov	z0\.d, z6\.d
**	mov	z1\.d, z4\.d
**	mov	z2\.d, z7\.d
**	mov	z3\.d, z5\.d
**	ret
*/
TEST_CREATE (create4_s8, svint8x4_t, svint8_t,
	     z0 = svcreate4_s8 (z6, z4, z7, z5),
	     z0 = svcreate4 (z6, z4, z7, z5))

/*
** create4_u8:
**	mov	z0\.d, z4\.d
**	mov	z1\.d, z6\.d
**	mov	z2\.d, z5\.d
**	mov	z3\.d, z7\.d
**	ret
*/
TEST_CREATE (create4_u8, svuint8x4_t, svuint8_t,
	     z0 = svcreate4_u8 (z4, z6, z5, z7),
	     z0 = svcreate4 (z4, z6, z5, z7))

/*
** create4_s16:
**	mov	z0\.d, z6\.d
**	mov	z1\.d, z4\.d
**	mov	z2\.d, z5\.d
**	mov	z3\.d, z7\.d
**	ret
*/
TEST_CREATE (create4_s16, svint16x4_t, svint16_t,
	     z0 = svcreate4_s16 (z6, z4, z5, z7),
	     z0 = svcreate4 (z6, z4, z5, z7))

/*
** create4_u16:
**	mov	z0\.d, z6\.d
**	mov	z1\.d, z5\.d
**	mov	z2\.d, z4\.d
**	mov	z3\.d, z7\.d
**	ret
*/
TEST_CREATE (create4_u16, svuint16x4_t, svuint16_t,
	     z0 = svcreate4_u16 (z6, z5, z4, z7),
	     z0 = svcreate4 (z6, z5, z4, z7))

/*
** create4_bf16:
**	mov	z0\.d, z4\.d
**	mov	z1\.d, z5\.d
**	mov	z2\.d, z6\.d
**	mov	z3\.d, z7\.d
**	ret
*/
TEST_CREATE (create4_bf16, svbfloat16x4_t, svbfloat16_t,
	     z0 = svcreate4_bf16 (z4, z5, z6, z7),
	     z0 = svcreate4 (z4, z5, z6, z7))

/*
** create4_f16:
**	mov	z0\.d, z4\.d
**	mov	z1\.d, z5\.d
**	mov	z2\.d, z6\.d
**	mov	z3\.d, z7\.d
**	ret
*/
TEST_CREATE (create4_f16, svfloat16x4_t, svfloat16_t,
	     z0 = svcreate4_f16 (z4, z5, z6, z7),
	     z0 = svcreate4 (z4, z5, z6, z7))

/*
** create4_s32:
**	mov	z0\.d, z6\.d
**	mov	z1\.d, z7\.d
**	mov	z2\.d, z4\.d
**	mov	z3\.d, z5\.d
**	ret
*/
TEST_CREATE (create4_s32, svint32x4_t, svint32_t,
	     z0 = svcreate4_s32 (z6, z7, z4, z5),
	     z0 = svcreate4 (z6, z7, z4, z5))

/*
** create4_u32:
**	mov	z0\.d, z7\.d
**	mov	z1\.d, z5\.d
**	mov	z2\.d, z6\.d
**	mov	z3\.d, z7\.d
**	ret
*/
TEST_CREATE (create4_u32, svuint32x4_t, svuint32_t,
	     z0 = svcreate4_u32 (z7, z5, z6, z7),
	     z0 = svcreate4 (z7, z5, z6, z7))

/*
** create4_f32:
**	mov	z0\.d, z7\.d
**	mov	z1\.d, z4\.d
**	mov	z2\.d, z6\.d
**	mov	z3\.d, z4\.d
**	ret
*/
TEST_CREATE (create4_f32, svfloat32x4_t, svfloat32_t,
	     z0 = svcreate4_f32 (z7, z4, z6, z4),
	     z0 = svcreate4 (z7, z4, z6, z4))

/*
** create4_s64:
**	mov	z0\.d, z5\.d
**	mov	z1\.d, z7\.d
**	mov	z2\.d, z6\.d
**	mov	z3\.d, z6\.d
**	ret
*/
TEST_CREATE (create4_s64, svint64x4_t, svint64_t,
	     z0 = svcreate4_s64 (z5, z7, z6, z6),
	     z0 = svcreate4 (z5, z7, z6, z6))

/*
** create4_u64:
**	mov	z0\.d, z7\.d
**	mov	z1\.d, z6\.d
**	mov	z2\.d, z4\.d
**	mov	z3\.d, z5\.d
**	ret
*/
TEST_CREATE (create4_u64, svuint64x4_t, svuint64_t,
	     z0 = svcreate4_u64 (z7, z6, z4, z5),
	     z0 = svcreate4 (z7, z6, z4, z5))

/*
** create4_f64:
**	mov	z0\.d, z5\.d
**	mov	z1\.d, z4\.d
**	mov	z2\.d, z7\.d
**	mov	z3\.d, z6\.d
**	ret
*/
TEST_CREATE (create4_f64, svfloat64x4_t, svfloat64_t,
	     z0 = svcreate4_f64 (z5, z4, z7, z6),
	     z0 = svcreate4 (z5, z4, z7, z6))
