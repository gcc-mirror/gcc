/* { dg-do assemble { target aarch64_asm_sve-b16b16_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve-b16b16_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve-b16b16"
#ifdef STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** mla_lane_0_bf16_tied1:
**	bfmla	z0\.h, z1\.h, z2\.h\[0\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_0_bf16_tied1, svbfloat16_t,
		z0 = svmla_lane_bf16 (z0, z1, z2, 0),
		z0 = svmla_lane (z0, z1, z2, 0))

/*
** mla_lane_0_bf16_tied2:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	bfmla	z0\.h, \1\.h, z2\.h\[0\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_0_bf16_tied2, svbfloat16_t,
		z0 = svmla_lane_bf16 (z1, z0, z2, 0),
		z0 = svmla_lane (z1, z0, z2, 0))

/*
** mla_lane_0_bf16_tied3:
**	mov	(z[0-9]+)\.d, z0\.d
**	movprfx	z0, z1
**	bfmla	z0\.h, z2\.h, \1\.h\[0\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_0_bf16_tied3, svbfloat16_t,
		z0 = svmla_lane_bf16 (z1, z2, z0, 0),
		z0 = svmla_lane (z1, z2, z0, 0))

/*
** mla_lane_0_bf16_untied:
**	movprfx	z0, z1
**	bfmla	z0\.h, z2\.h, z3\.h\[0\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_0_bf16_untied, svbfloat16_t,
		z0 = svmla_lane_bf16 (z1, z2, z3, 0),
		z0 = svmla_lane (z1, z2, z3, 0))

/*
** mla_lane_1_bf16:
**	bfmla	z0\.h, z1\.h, z2\.h\[1\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_1_bf16, svbfloat16_t,
		z0 = svmla_lane_bf16 (z0, z1, z2, 1),
		z0 = svmla_lane (z0, z1, z2, 1))

/*
** mla_lane_2_bf16:
**	bfmla	z0\.h, z1\.h, z2\.h\[2\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_2_bf16, svbfloat16_t,
		z0 = svmla_lane_bf16 (z0, z1, z2, 2),
		z0 = svmla_lane (z0, z1, z2, 2))

/*
** mla_lane_3_bf16:
**	bfmla	z0\.h, z1\.h, z2\.h\[3\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_3_bf16, svbfloat16_t,
		z0 = svmla_lane_bf16 (z0, z1, z2, 3),
		z0 = svmla_lane (z0, z1, z2, 3))

/*
** mla_lane_4_bf16:
**	bfmla	z0\.h, z1\.h, z2\.h\[4\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_4_bf16, svbfloat16_t,
		z0 = svmla_lane_bf16 (z0, z1, z2, 4),
		z0 = svmla_lane (z0, z1, z2, 4))

/*
** mla_lane_5_bf16:
**	bfmla	z0\.h, z1\.h, z2\.h\[5\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_5_bf16, svbfloat16_t,
		z0 = svmla_lane_bf16 (z0, z1, z2, 5),
		z0 = svmla_lane (z0, z1, z2, 5))

/*
** mla_lane_6_bf16:
**	bfmla	z0\.h, z1\.h, z2\.h\[6\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_6_bf16, svbfloat16_t,
		z0 = svmla_lane_bf16 (z0, z1, z2, 6),
		z0 = svmla_lane (z0, z1, z2, 6))

/*
** mla_lane_7_bf16:
**	bfmla	z0\.h, z1\.h, z2\.h\[7\]
**	ret
*/
TEST_UNIFORM_Z (mla_lane_7_bf16, svbfloat16_t,
		z0 = svmla_lane_bf16 (z0, z1, z2, 7),
		z0 = svmla_lane (z0, z1, z2, 7))

/*
** mla_lane_z7_bf16:
**	bfmla	z0\.h, z1\.h, z7\.h\[7\]
**	ret
*/
TEST_DUAL_Z (mla_lane_z7_bf16, svbfloat16_t, svbfloat16_t,
	     z0 = svmla_lane_bf16 (z0, z1, z7, 7),
	     z0 = svmla_lane (z0, z1, z7, 7))

/*
** mla_lane_z8_bf16:
**	str	d8, \[sp, -16\]!
**	mov	(z[0-7])\.d, z8\.d
**	bfmla	z0\.h, z1\.h, \1\.h\[7\]
**	ldr	d8, \[sp\], 16
**	ret
*/
TEST_DUAL_LANE_REG (mla_lane_z8_bf16, svbfloat16_t, svbfloat16_t, z8,
		    z0 = svmla_lane_bf16 (z0, z1, z8, 7),
		    z0 = svmla_lane (z0, z1, z8, 7))
