/* { dg-do assemble { target aarch64_asm_lut_ok } } */
/* { dg-do compile { target { ! aarch64_asm_lut_ok } } } */
/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

#pragma GCC target "+sve2+lut"
#if STREAMING_COMPATIBLE
#pragma GCC target "+sme2"
#endif

/*
** luti4_min_idx_test:
**	 luti4	z1\.h, \{ z28\.h \}, z0\[0\]
**	 ret
*/

TEST_XN_SINGLE (luti4_min_idx_test, svbfloat16_t, svuint8_t, z1,
                svluti4_lane_bf16 (z28, z0, 0),
                svluti4_lane (z28, z0, 0))

/*
** luti4_max_idx_test:
**	 luti4	z1\.h, \{ z28\.h \}, z0\[3\]
**	 ret
*/

TEST_XN_SINGLE (luti4_max_idx_test, svbfloat16_t, svuint8_t, z1,
                svluti4_lane_bf16 (z28, z0, 3),
                svluti4_lane (z28, z0, 3))

/*
** luti4_tied_min_idx_test:
**	 luti4	z28\.h, \{ z28\.h \}, z0\[0\]
**	 ret
*/

TEST_XN_SINGLE (luti4_tied_min_idx_test, svbfloat16_t, svuint8_t, z28,
                svluti4_lane_bf16 (z28, z0, 0),
                svluti4_lane (z28, z0, 0))

/*
** luti4_tied_max_idx_test:
**	 luti4	z28\.h, \{ z28\.h \}, z0\[3\]
**	 ret
*/

TEST_XN_SINGLE (luti4_tied_max_idx_test, svbfloat16_t, svuint8_t, z28,
                svluti4_lane_bf16 (z28, z0, 3),
                svluti4_lane (z28, z0, 3))
