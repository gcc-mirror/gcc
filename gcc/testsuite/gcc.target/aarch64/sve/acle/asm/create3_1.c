/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** create3_s8:
**	mov	z0\.d, z6\.d
**	mov	z1\.d, z4\.d
**	mov	z2\.d, z7\.d
**	ret
*/
TEST_CREATE (create3_s8, svint8x3_t, svint8_t,
	     z0 = svcreate3_s8 (z6, z4, z7),
	     z0 = svcreate3 (z6, z4, z7))

/*
** create3_u8:
**	mov	z0\.d, z4\.d
**	mov	z1\.d, z6\.d
**	mov	z2\.d, z5\.d
**	ret
*/
TEST_CREATE (create3_u8, svuint8x3_t, svuint8_t,
	     z0 = svcreate3_u8 (z4, z6, z5),
	     z0 = svcreate3 (z4, z6, z5))

/*
** create3_s16:
**	mov	z0\.d, z6\.d
**	mov	z1\.d, z4\.d
**	mov	z2\.d, z5\.d
**	ret
*/
TEST_CREATE (create3_s16, svint16x3_t, svint16_t,
	     z0 = svcreate3_s16 (z6, z4, z5),
	     z0 = svcreate3 (z6, z4, z5))

/*
** create3_u16:
**	mov	z0\.d, z6\.d
**	mov	z1\.d, z5\.d
**	mov	z2\.d, z4\.d
**	ret
*/
TEST_CREATE (create3_u16, svuint16x3_t, svuint16_t,
	     z0 = svcreate3_u16 (z6, z5, z4),
	     z0 = svcreate3 (z6, z5, z4))

/*
** create3_f16:
**	mov	z0\.d, z4\.d
**	mov	z1\.d, z5\.d
**	mov	z2\.d, z6\.d
**	ret
*/
TEST_CREATE (create3_f16, svfloat16x3_t, svfloat16_t,
	     z0 = svcreate3_f16 (z4, z5, z6),
	     z0 = svcreate3 (z4, z5, z6))

/*
** create3_s32:
**	mov	z0\.d, z6\.d
**	mov	z1\.d, z7\.d
**	mov	z2\.d, z4\.d
**	ret
*/
TEST_CREATE (create3_s32, svint32x3_t, svint32_t,
	     z0 = svcreate3_s32 (z6, z7, z4),
	     z0 = svcreate3 (z6, z7, z4))

/*
** create3_u32:
**	mov	z0\.d, z7\.d
**	mov	z1\.d, z5\.d
**	mov	z2\.d, z6\.d
**	ret
*/
TEST_CREATE (create3_u32, svuint32x3_t, svuint32_t,
	     z0 = svcreate3_u32 (z7, z5, z6),
	     z0 = svcreate3 (z7, z5, z6))

/*
** create3_f32:
**	mov	z0\.d, z7\.d
**	mov	z1\.d, z4\.d
**	mov	z2\.d, z6\.d
**	ret
*/
TEST_CREATE (create3_f32, svfloat32x3_t, svfloat32_t,
	     z0 = svcreate3_f32 (z7, z4, z6),
	     z0 = svcreate3 (z7, z4, z6))

/*
** create3_s64:
**	mov	z0\.d, z5\.d
**	mov	z1\.d, z7\.d
**	mov	z2\.d, z6\.d
**	ret
*/
TEST_CREATE (create3_s64, svint64x3_t, svint64_t,
	     z0 = svcreate3_s64 (z5, z7, z6),
	     z0 = svcreate3 (z5, z7, z6))

/*
** create3_u64:
**	mov	z0\.d, z7\.d
**	mov	z1\.d, z6\.d
**	mov	z2\.d, z4\.d
**	ret
*/
TEST_CREATE (create3_u64, svuint64x3_t, svuint64_t,
	     z0 = svcreate3_u64 (z7, z6, z4),
	     z0 = svcreate3 (z7, z6, z4))

/*
** create3_f64:
**	mov	z0\.d, z5\.d
**	mov	z1\.d, z4\.d
**	mov	z2\.d, z7\.d
**	ret
*/
TEST_CREATE (create3_f64, svfloat64x3_t, svfloat64_t,
	     z0 = svcreate3_f64 (z5, z4, z7),
	     z0 = svcreate3 (z5, z4, z7))
