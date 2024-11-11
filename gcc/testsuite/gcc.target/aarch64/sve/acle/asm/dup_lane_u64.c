/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dup_lane_x0_u64_tied1:
**	mov	(z[0-9]+\.d), x0
**	tbl	z0\.d, {z0\.d}, \1
**	ret
*/
TEST_UNIFORM_ZX (dup_lane_x0_u64_tied1, svuint64_t, uint64_t,
		 z0 = svdup_lane_u64 (z0, x0),
		 z0 = svdup_lane (z0, x0))

/*
** dup_lane_x0_u64_untied:
**	mov	(z[0-9]+\.d), x0
**	tbl	z0\.d, {z1\.d}, \1
**	ret
*/
TEST_UNIFORM_ZX (dup_lane_x0_u64_untied, svuint64_t, uint64_t,
		 z0 = svdup_lane_u64 (z1, x0),
		 z0 = svdup_lane (z1, x0))

/*
** dup_lane_0_u64_tied1:
**	dup	z0\.d, z0\.d\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_lane_0_u64_tied1, svuint64_t,
		z0 = svdup_lane_u64 (z0, 0),
		z0 = svdup_lane (z0, 0))

/*
** dup_lane_0_u64_untied:
**	dup	z0\.d, z1\.d\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_lane_0_u64_untied, svuint64_t,
		z0 = svdup_lane_u64 (z1, 0),
		z0 = svdup_lane (z1, 0))

/*
** dup_lane_7_u64:
**	dup	z0\.d, z0\.d\[7\]
**	ret
*/
TEST_UNIFORM_Z (dup_lane_7_u64, svuint64_t,
		z0 = svdup_lane_u64 (z0, 7),
		z0 = svdup_lane (z0, 7))

/*
** dup_lane_8_u64:
**	mov	(z[0-9]+\.d), #8
**	tbl	z0\.d, {z0\.d}, \1
**	ret
*/
TEST_UNIFORM_Z (dup_lane_8_u64, svuint64_t,
		z0 = svdup_lane_u64 (z0, 8),
		z0 = svdup_lane (z0, 8))

/*
** dup_lane_15_u64:
**	mov	(z[0-9]+\.d), #15
**	tbl	z0\.d, {z0\.d}, \1
**	ret
*/
TEST_UNIFORM_Z (dup_lane_15_u64, svuint64_t,
		z0 = svdup_lane_u64 (z0, 15),
		z0 = svdup_lane (z0, 15))

/*
** dup_lane_16_u64:
**	mov	(z[0-9]+\.d), #16
**	tbl	z0\.d, {z0\.d}, \1
**	ret
*/
TEST_UNIFORM_Z (dup_lane_16_u64, svuint64_t,
		z0 = svdup_lane_u64 (z0, 16),
		z0 = svdup_lane (z0, 16))

/*
** dup_lane_31_u64:
**	mov	(z[0-9]+\.d), #31
**	tbl	z0\.d, {z0\.d}, \1
**	ret
*/
TEST_UNIFORM_Z (dup_lane_31_u64, svuint64_t,
		z0 = svdup_lane_u64 (z0, 31),
		z0 = svdup_lane (z0, 31))

/*
** dup_lane_32_u64:
**	mov	(z[0-9]+\.d), #32
**	tbl	z0\.d, {z0\.d}, \1
**	ret
*/
TEST_UNIFORM_Z (dup_lane_32_u64, svuint64_t,
		z0 = svdup_lane_u64 (z0, 32),
		z0 = svdup_lane (z0, 32))

/*
** dup_lane_63_u64:
**	mov	(z[0-9]+\.d), #63
**	tbl	z0\.d, {z0\.d}, \1
**	ret
*/
TEST_UNIFORM_Z (dup_lane_63_u64, svuint64_t,
		z0 = svdup_lane_u64 (z0, 63),
		z0 = svdup_lane (z0, 63))

/*
** dup_lane_64_u64:
**	mov	(z[0-9]+\.d), #64
**	tbl	z0\.d, {z0\.d}, \1
**	ret
*/
TEST_UNIFORM_Z (dup_lane_64_u64, svuint64_t,
		z0 = svdup_lane_u64 (z0, 64),
		z0 = svdup_lane (z0, 64))

/*
** dup_lane_255_u64:
**	mov	(z[0-9]+\.d), #255
**	tbl	z0\.d, {z0\.d}, \1
**	ret
*/
TEST_UNIFORM_Z (dup_lane_255_u64, svuint64_t,
		z0 = svdup_lane_u64 (z0, 255),
		z0 = svdup_lane (z0, 255))
