/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dup_lane_w0_u32_tied1:
**	mov	(z[0-9]+\.s), w0
**	tbl	z0\.s, {z0\.s}, \1
**	ret
*/
TEST_UNIFORM_ZX (dup_lane_w0_u32_tied1, svuint32_t, uint32_t,
		 z0 = svdup_lane_u32 (z0, x0),
		 z0 = svdup_lane (z0, x0))

/*
** dup_lane_w0_u32_untied:
**	mov	(z[0-9]+\.s), w0
**	tbl	z0\.s, {z1\.s}, \1
**	ret
*/
TEST_UNIFORM_ZX (dup_lane_w0_u32_untied, svuint32_t, uint32_t,
		 z0 = svdup_lane_u32 (z1, x0),
		 z0 = svdup_lane (z1, x0))

/*
** dup_lane_0_u32_tied1:
**	dup	z0\.s, z0\.s\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_lane_0_u32_tied1, svuint32_t,
		z0 = svdup_lane_u32 (z0, 0),
		z0 = svdup_lane (z0, 0))

/*
** dup_lane_0_u32_untied:
**	dup	z0\.s, z1\.s\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_lane_0_u32_untied, svuint32_t,
		z0 = svdup_lane_u32 (z1, 0),
		z0 = svdup_lane (z1, 0))

/*
** dup_lane_7_u32:
**	dup	z0\.s, z0\.s\[7\]
**	ret
*/
TEST_UNIFORM_Z (dup_lane_7_u32, svuint32_t,
		z0 = svdup_lane_u32 (z0, 7),
		z0 = svdup_lane (z0, 7))

/*
** dup_lane_8_u32:
**	dup	z0\.s, z0\.s\[8\]
**	ret
*/
TEST_UNIFORM_Z (dup_lane_8_u32, svuint32_t,
		z0 = svdup_lane_u32 (z0, 8),
		z0 = svdup_lane (z0, 8))

/*
** dup_lane_15_u32:
**	dup	z0\.s, z0\.s\[15\]
**	ret
*/
TEST_UNIFORM_Z (dup_lane_15_u32, svuint32_t,
		z0 = svdup_lane_u32 (z0, 15),
		z0 = svdup_lane (z0, 15))

/*
** dup_lane_16_u32:
**	mov	(z[0-9]+\.s), #16
**	tbl	z0\.s, {z0\.s}, \1
**	ret
*/
TEST_UNIFORM_Z (dup_lane_16_u32, svuint32_t,
		z0 = svdup_lane_u32 (z0, 16),
		z0 = svdup_lane (z0, 16))

/*
** dup_lane_31_u32:
**	mov	(z[0-9]+\.s), #31
**	tbl	z0\.s, {z0\.s}, \1
**	ret
*/
TEST_UNIFORM_Z (dup_lane_31_u32, svuint32_t,
		z0 = svdup_lane_u32 (z0, 31),
		z0 = svdup_lane (z0, 31))

/*
** dup_lane_32_u32:
**	mov	(z[0-9]+\.s), #32
**	tbl	z0\.s, {z0\.s}, \1
**	ret
*/
TEST_UNIFORM_Z (dup_lane_32_u32, svuint32_t,
		z0 = svdup_lane_u32 (z0, 32),
		z0 = svdup_lane (z0, 32))

/*
** dup_lane_63_u32:
**	mov	(z[0-9]+\.s), #63
**	tbl	z0\.s, {z0\.s}, \1
**	ret
*/
TEST_UNIFORM_Z (dup_lane_63_u32, svuint32_t,
		z0 = svdup_lane_u32 (z0, 63),
		z0 = svdup_lane (z0, 63))

/*
** dup_lane_64_u32:
**	mov	(z[0-9]+\.s), #64
**	tbl	z0\.s, {z0\.s}, \1
**	ret
*/
TEST_UNIFORM_Z (dup_lane_64_u32, svuint32_t,
		z0 = svdup_lane_u32 (z0, 64),
		z0 = svdup_lane (z0, 64))

/*
** dup_lane_255_u32:
**	mov	(z[0-9]+\.s), #255
**	tbl	z0\.s, {z0\.s}, \1
**	ret
*/
TEST_UNIFORM_Z (dup_lane_255_u32, svuint32_t,
		z0 = svdup_lane_u32 (z0, 255),
		z0 = svdup_lane (z0, 255))
