/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vqshl_n_s8_3:
** sqshl	v0\.8b, v0\.8b, 3
** ret
*/
TEST_UNIFORM_UNARY_N (vqshl_n_s8, int8x8_t, 3)

/*
** test_vqshlq_n_u16_15:
** uqshl	v0\.8h, v0\.8h, 15
** ret
*/
TEST_UNIFORM_UNARY_N (vqshlq_n_u16, uint16x8_t, 15)

/*
** test_vqshl_n_s64_7:
** sqshl	d0, d0, 7
** ret
*/
TEST_UNIFORM_UNARY_N (vqshl_n_s64, int64x1_t, 7)

/*
** test_vqshlq_n_s64_0:
** sqshl	v0\.2d, v0\.2d, 0
** ret
*/
TEST_UNIFORM_UNARY_N (vqshlq_n_s64, int64x2_t, 0)

/*
** test_vqshlu_n_s8_3:
** sqshlu	v0\.8b, v0\.8b, 3
** ret
*/
TEST_UNARY_N (vqshlu_n_s8, uint8x8_t, int8x8_t, 3)

/*
** test_vqshluq_n_s32_31:
** sqshlu	v0\.4s, v0\.4s, 31
** ret
*/
TEST_UNARY_N (vqshluq_n_s32, uint32x4_t, int32x4_t, 31)

/*
** test_vqshlu_n_s64_2:
** sqshlu	d0, d0, 2
** ret
*/
TEST_UNARY_N (vqshlu_n_s64, uint64x1_t, int64x1_t, 2)

/*
** test_vqshlh_n_s16_3:
** dup	v[0-9]+\.4h, w0
** sqshl	h[0-9]+, h[0-9]+, 3
** umov	w0, v[0-9]+\.h\[0\]
** ret
*/
TEST_UNARY_N (vqshlh_n_s16, int16_t, int16_t, 3)

/*
** test_vqshlub_n_s8_3:
** dup	v[0-9]+\.8b, w0
** sqshlu	b[0-9]+, b[0-9]+, 3
** umov	w0, v[0-9]+\.b\[0\]
** ret
*/
TEST_UNARY_N (vqshlub_n_s8, uint8_t, int8_t, 3)
