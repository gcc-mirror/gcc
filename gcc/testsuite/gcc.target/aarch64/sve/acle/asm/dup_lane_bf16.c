/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** dup_lane_w0_bf16_tied1:
**	mov	(z[0-9]+\.h), w0
**	tbl	z0\.h, {z0\.h}, \1
**	ret
*/
TEST_UNIFORM_ZX (dup_lane_w0_bf16_tied1, svbfloat16_t, uint16_t,
		 z0 = svdup_lane_bf16 (z0, x0),
		 z0 = svdup_lane (z0, x0))

/*
** dup_lane_w0_bf16_untied:
**	mov	(z[0-9]+\.h), w0
**	tbl	z0\.h, {z1\.h}, \1
**	ret
*/
TEST_UNIFORM_ZX (dup_lane_w0_bf16_untied, svbfloat16_t, uint16_t,
		 z0 = svdup_lane_bf16 (z1, x0),
		 z0 = svdup_lane (z1, x0))

/*
** dup_lane_0_bf16_tied1:
**	dup	z0\.h, z0\.h\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_lane_0_bf16_tied1, svbfloat16_t,
		z0 = svdup_lane_bf16 (z0, 0),
		z0 = svdup_lane (z0, 0))

/*
** dup_lane_0_bf16_untied:
**	dup	z0\.h, z1\.h\[0\]
**	ret
*/
TEST_UNIFORM_Z (dup_lane_0_bf16_untied, svbfloat16_t,
		z0 = svdup_lane_bf16 (z1, 0),
		z0 = svdup_lane (z1, 0))

/*
** dup_lane_15_bf16:
**	dup	z0\.h, z0\.h\[15\]
**	ret
*/
TEST_UNIFORM_Z (dup_lane_15_bf16, svbfloat16_t,
		z0 = svdup_lane_bf16 (z0, 15),
		z0 = svdup_lane (z0, 15))

/*
** dup_lane_16_bf16:
**	dup	z0\.h, z0\.h\[16\]
**	ret
*/
TEST_UNIFORM_Z (dup_lane_16_bf16, svbfloat16_t,
		z0 = svdup_lane_bf16 (z0, 16),
		z0 = svdup_lane (z0, 16))

/*
** dup_lane_31_bf16:
**	dup	z0\.h, z0\.h\[31\]
**	ret
*/
TEST_UNIFORM_Z (dup_lane_31_bf16, svbfloat16_t,
		z0 = svdup_lane_bf16 (z0, 31),
		z0 = svdup_lane (z0, 31))

/*
** dup_lane_32_bf16:
**	mov	(z[0-9]+\.h), #32
**	tbl	z0\.h, {z0\.h}, \1
**	ret
*/
TEST_UNIFORM_Z (dup_lane_32_bf16, svbfloat16_t,
		z0 = svdup_lane_bf16 (z0, 32),
		z0 = svdup_lane (z0, 32))

/*
** dup_lane_63_bf16:
**	mov	(z[0-9]+\.h), #63
**	tbl	z0\.h, {z0\.h}, \1
**	ret
*/
TEST_UNIFORM_Z (dup_lane_63_bf16, svbfloat16_t,
		z0 = svdup_lane_bf16 (z0, 63),
		z0 = svdup_lane (z0, 63))

/*
** dup_lane_64_bf16:
**	mov	(z[0-9]+\.h), #64
**	tbl	z0\.h, {z0\.h}, \1
**	ret
*/
TEST_UNIFORM_Z (dup_lane_64_bf16, svbfloat16_t,
		z0 = svdup_lane_bf16 (z0, 64),
		z0 = svdup_lane (z0, 64))

/*
** dup_lane_255_bf16:
**	mov	(z[0-9]+\.h), #255
**	tbl	z0\.h, {z0\.h}, \1
**	ret
*/
TEST_UNIFORM_Z (dup_lane_255_bf16, svbfloat16_t,
		z0 = svdup_lane_bf16 (z0, 255),
		z0 = svdup_lane (z0, 255))
