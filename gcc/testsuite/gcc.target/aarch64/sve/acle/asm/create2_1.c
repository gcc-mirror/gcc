/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** create2_s8:
** (
**	mov	z0\.d, z6\.d
**	mov	z1\.d, z4\.d
** |
**	mov	z1\.d, z4\.d
**	mov	z0\.d, z6\.d
** )
**	ret
*/
TEST_CREATE (create2_s8, svint8x2_t, svint8_t,
	     z0 = svcreate2_s8 (z6, z4),
	     z0 = svcreate2 (z6, z4))

/*
** create2_u8:
** (
**	mov	z0\.d, z4\.d
**	mov	z1\.d, z6\.d
** |
**	mov	z1\.d, z6\.d
**	mov	z0\.d, z4\.d
** )
**	ret
*/
TEST_CREATE (create2_u8, svuint8x2_t, svuint8_t,
	     z0 = svcreate2_u8 (z4, z6),
	     z0 = svcreate2 (z4, z6))

/*
** create2_s16:
** (
**	mov	z0\.d, z6\.d
**	mov	z1\.d, z4\.d
** |
**	mov	z1\.d, z4\.d
**	mov	z0\.d, z6\.d
** )
**	ret
*/
TEST_CREATE (create2_s16, svint16x2_t, svint16_t,
	     z0 = svcreate2_s16 (z6, z4),
	     z0 = svcreate2 (z6, z4))

/*
** create2_u16:
** (
**	mov	z0\.d, z6\.d
**	mov	z1\.d, z5\.d
** |
**	mov	z1\.d, z5\.d
**	mov	z0\.d, z6\.d
** )
**	ret
*/
TEST_CREATE (create2_u16, svuint16x2_t, svuint16_t,
	     z0 = svcreate2_u16 (z6, z5),
	     z0 = svcreate2 (z6, z5))

/*
** create2_bf16:
** (
**	mov	z0\.d, z4\.d
**	mov	z1\.d, z5\.d
** |
**	mov	z1\.d, z5\.d
**	mov	z0\.d, z4\.d
** )
**	ret
*/
TEST_CREATE (create2_bf16, svbfloat16x2_t, svbfloat16_t,
	     z0 = svcreate2_bf16 (z4, z5),
	     z0 = svcreate2 (z4, z5))

/*
** create2_f16:
** (
**	mov	z0\.d, z4\.d
**	mov	z1\.d, z5\.d
** |
**	mov	z1\.d, z5\.d
**	mov	z0\.d, z4\.d
** )
**	ret
*/
TEST_CREATE (create2_f16, svfloat16x2_t, svfloat16_t,
	     z0 = svcreate2_f16 (z4, z5),
	     z0 = svcreate2 (z4, z5))

/*
** create2_s32:
** (
**	mov	z0\.d, z6\.d
**	mov	z1\.d, z7\.d
** |
**	mov	z1\.d, z7\.d
**	mov	z0\.d, z6\.d
** )
**	ret
*/
TEST_CREATE (create2_s32, svint32x2_t, svint32_t,
	     z0 = svcreate2_s32 (z6, z7),
	     z0 = svcreate2 (z6, z7))

/*
** create2_u32:
** (
**	mov	z0\.d, z7\.d
**	mov	z1\.d, z5\.d
** |
**	mov	z1\.d, z5\.d
**	mov	z0\.d, z7\.d
** )
**	ret
*/
TEST_CREATE (create2_u32, svuint32x2_t, svuint32_t,
	     z0 = svcreate2_u32 (z7, z5),
	     z0 = svcreate2 (z7, z5))

/*
** create2_f32:
** (
**	mov	z0\.d, z7\.d
**	mov	z1\.d, z4\.d
** |
**	mov	z1\.d, z4\.d
**	mov	z0\.d, z7\.d
** )
**	ret
*/
TEST_CREATE (create2_f32, svfloat32x2_t, svfloat32_t,
	     z0 = svcreate2_f32 (z7, z4),
	     z0 = svcreate2 (z7, z4))

/*
** create2_s64:
** (
**	mov	z0\.d, z5\.d
**	mov	z1\.d, z7\.d
** |
**	mov	z1\.d, z7\.d
**	mov	z0\.d, z5\.d
** )
**	ret
*/
TEST_CREATE (create2_s64, svint64x2_t, svint64_t,
	     z0 = svcreate2_s64 (z5, z7),
	     z0 = svcreate2 (z5, z7))

/*
** create2_u64:
** (
**	mov	z0\.d, z7\.d
**	mov	z1\.d, z6\.d
** |
**	mov	z1\.d, z6\.d
**	mov	z0\.d, z7\.d
** )
**	ret
*/
TEST_CREATE (create2_u64, svuint64x2_t, svuint64_t,
	     z0 = svcreate2_u64 (z7, z6),
	     z0 = svcreate2 (z7, z6))

/*
** create2_f64:
** (
**	mov	z0\.d, z5\.d
**	mov	z1\.d, z4\.d
** |
**	mov	z1\.d, z4\.d
**	mov	z0\.d, z5\.d
** )
**	ret
*/
TEST_CREATE (create2_f64, svfloat64x2_t, svfloat64_t,
	     z0 = svcreate2_f64 (z5, z4),
	     z0 = svcreate2 (z5, z4))

/*
** create2_b_0:
**	ret
*/
TEST_CREATE_B (create2_b_0, svboolx2_t,
	       p0_res = svcreate2_b (p0, p1),
	       p0_res = svcreate2 (p0, p1))

/*
** create2_b_1:
** (
**	mov	p0\.b, p2\.b
**	mov	p1\.b, p3\.b
** |
**	mov	p1\.b, p3\.b
**	mov	p0\.b, p2\.b
** )
**	ret
*/
TEST_CREATE_B (create2_b_1, svboolx2_t,
	       p0_res = svcreate2_b (p2, p3),
	       p0_res = svcreate2 (p2, p3))
