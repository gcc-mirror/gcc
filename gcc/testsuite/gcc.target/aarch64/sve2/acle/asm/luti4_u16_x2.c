/* { dg-do assemble { target aarch64_asm_lut_ok } } */
/* { dg-do compile { target { ! aarch64_asm_lut_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2+lut"
#if STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** luti4_min_idx_aligned_test:
**	luti4	z0\.h, \{z6\.h \- z7\.h\}, z22\[0\]
**	ret
*/

TEST_1X2_NARROW(luti4_min_idx_aligned_test, svuint16_t, svuint16x2_t, svuint8_t,
		z0 = svluti4_lane_u16_x2 (z6, z22, 0),
		z0 = svluti4_lane (z6, z22, 0))

/*
** luti4_max_idx_aligned_test:
**	luti4	z0\.h, \{z16\.h \- z17\.h\}, z22\[3\]
**	ret
*/

TEST_1X2_NARROW(luti4_max_idx_aligned_test, svuint16_t, svuint16x2_t, svuint8_t,
		z0_res = svluti4_lane_u16_x2 (z16, z22, 3),
		z0_res = svluti4_lane (z16, z22, 3))

/*
** luti4_min_idx_misaligned_test:
**	luti4	z0\.h, \{z5\.h \- z6\.h\}, z22\[0\]
**	ret
*/

TEST_1X2_NARROW(luti4_min_idx_misaligned_test, svuint16_t, svuint16x2_t, svuint8_t,
		z0 = svluti4_lane_u16_x2 (z5, z22, 0),
		z0 = svluti4_lane (z5, z22, 0))

/*
** luti4_max_idx_misaligned_test:
**	luti4	z0\.h, \{z29\.h \- z30\.h\}, z22\[3\]
**	ret
*/

TEST_1X2_NARROW(luti4_max_idx_misaligned_test, svuint16_t, svuint16x2_t, svuint8_t,
		z0_res = svluti4_lane_u16_x2 (z29, z22, 3),
		z0_res = svluti4_lane (z29, z22, 3))
