/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vsra_n_s8_3:
** ssra	v0\.8b, v1\.8b, 3
** ret
*/
TEST_UNIFORM_BINARY_N (vsra_n_s8, int8x8_t, 3)

/*
** test_vsraq_n_u32_5:
** usra	v0\.4s, v1\.4s, 5
** ret
*/
TEST_UNIFORM_BINARY_N (vsraq_n_u32, uint32x4_t, 5)

/*
** test_vsraq_n_s64_63:
** ssra	v0\.2d, v1\.2d, 63
** ret
*/
TEST_UNIFORM_BINARY_N (vsraq_n_s64, int64x2_t, 63)

/*
** test_vsraq_n_u32_32:
** ret
*/
TEST_UNIFORM_BINARY_N (vsraq_n_u32, uint32x4_t, 32)

/*
** test_vsraq_n_s32_32:
** ssra	v0\.4s, v1\.4s, 31
** ret
*/
TEST_UNIFORM_BINARY_N (vsraq_n_s32, int32x4_t, 32)

/*
** test_vsrad_n_s64_3:
** add	x0, x0, x1, asr 3
** ret
*/
TEST_UNIFORM_BINARY_N (vsrad_n_s64, int64_t, 3)

/*
** test_vsrad_n_u64_5:
** add	x0, x0, x1, lsr 5
** ret
*/
TEST_UNIFORM_BINARY_N (vsrad_n_u64, uint64_t, 5)
