/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vshrn_n_s16_3:
** shrn	v0\.8b, v0\.8h, 3
** ret
*/
TEST_UNARY_N (vshrn_n_s16, int8x8_t, int16x8_t, 3)

/*
** test_vrshrn_n_u32_16:
** rshrn	v0\.4h, v0\.4s, 16
** ret
*/
TEST_UNARY_N (vrshrn_n_u32, uint16x4_t, uint32x4_t, 16)

/*
** test_vqshrn_n_s64_17:
** sqshrn	v0\.2s, v0\.2d, 17
** ret
*/
TEST_UNARY_N (vqshrn_n_s64, int32x2_t, int64x2_t, 17)

/*
** test_vqshrn_n_u16_8:
** uqshrn	v0\.8b, v0\.8h, 8
** ret
*/
TEST_UNARY_N (vqshrn_n_u16, uint8x8_t, uint16x8_t, 8)

/*
** test_vqrshrn_n_s32_9:
** sqrshrn	v0\.4h, v0\.4s, 9
** ret
*/
TEST_UNARY_N (vqrshrn_n_s32, int16x4_t, int32x4_t, 9)

/*
** test_vqshrun_n_s16_3:
** sqshrun	v0\.8b, v0\.8h, 3
** ret
*/
TEST_UNARY_N (vqshrun_n_s16, uint8x8_t, int16x8_t, 3)

/*
** test_vqrshrun_n_s64_17:
** sqrshrun	v0\.2s, v0\.2d, 17
** ret
*/
TEST_UNARY_N (vqrshrun_n_s64, uint32x2_t, int64x2_t, 17)

/*
** test_vshrn_high_n_u16_3:
** shrn2	v0\.16b, v1\.8h, 3
** ret
*/
TEST_BINARY_N (vshrn_high_n_u16, uint8x16_t, uint8x8_t, uint16x8_t, 3)

/*
** test_vrshrn_high_n_s32_9:
** rshrn2	v0\.8h, v1\.4s, 9
** ret
*/
TEST_BINARY_N (vrshrn_high_n_s32, int16x8_t, int16x4_t, int32x4_t, 9)

/*
** test_vqshrn_high_n_s16_3:
** sqshrn2	v0\.16b, v1\.8h, 3
** ret
*/
TEST_BINARY_N (vqshrn_high_n_s16, int8x16_t, int8x8_t, int16x8_t, 3)

/*
** test_vqshrn_high_n_u64_17:
** uqshrn2	v0\.4s, v1\.2d, 17
** ret
*/
TEST_BINARY_N (vqshrn_high_n_u64, uint32x4_t, uint32x2_t, uint64x2_t, 17)

/*
** test_vqrshrn_high_n_s32_9:
** sqrshrn2	v0\.8h, v1\.4s, 9
** ret
*/
TEST_BINARY_N (vqrshrn_high_n_s32, int16x8_t, int16x4_t, int32x4_t, 9)

/*
** test_vqshrun_high_n_s16_3:
** sqshrun2	v0\.16b, v1\.8h, 3
** ret
*/
TEST_BINARY_N (vqshrun_high_n_s16, uint8x16_t, uint8x8_t, int16x8_t, 3)

/*
** test_vqrshrun_high_n_s64_17:
** sqrshrun2	v0\.4s, v1\.2d, 17
** ret
*/
TEST_BINARY_N (vqrshrun_high_n_s64, uint32x4_t, uint32x2_t, int64x2_t, 17)

/*
** test_vqshrnh_n_s16_3:
** dup	v[0-9]+\.4h, w0
** sqshrn	b[0-9]+, h[0-9]+, 3
** umov	w0, v[0-9]+\.b\[0\]
** ret
*/
TEST_UNARY_N (vqshrnh_n_s16, int8_t, int16_t, 3)

/*
** test_vqrshrns_n_u32_9:
** fmov	s[0-9]+, w0
** uqrshrn	h[0-9]+, s[0-9]+, 9
** umov	w0, v[0-9]+\.h\[0\]
** ret
*/
TEST_UNARY_N (vqrshrns_n_u32, uint16_t, uint32_t, 9)

/*
** test_vqshrunh_n_s16_3:
** dup	v[0-9]+\.4h, w0
** sqshrun	b[0-9]+, h[0-9]+, 3
** umov	w0, v[0-9]+\.h\[0\]
** ret
*/
TEST_UNARY_N (vqshrunh_n_s16, uint8_t, int16_t, 3)
