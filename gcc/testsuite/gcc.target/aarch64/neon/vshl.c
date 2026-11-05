/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

#define TEST_SHIFT_CST(NAME, TYPE, DUP, AMOUNT)                                \
  TYPE test_##NAME##_cst (TYPE a) { return NAME (a, DUP (AMOUNT)); }

/*
** test_vshl_s8:
** sshl	v0\.8b, v0\.8b, v1\.8b
** ret
*/
TEST_UNIFORM_BINARY (vshl_s8, int8x8_t)

/*
** test_vshlq_u32:
** ushl	v0\.4s, v0\.4s, v1\.4s
** ret
*/
TEST_BINARY (vshlq_u32, uint32x4_t, uint32x4_t, int32x4_t)

/*
** test_vshl_s64:
** sshl	d0, d0, d1
** ret
*/
TEST_UNIFORM_BINARY (vshl_s64, int64x1_t)

/*
** test_vrshlq_s16:
** srshl	v0\.8h, v0\.8h, v1\.8h
** ret
*/
TEST_UNIFORM_BINARY (vrshlq_s16, int16x8_t)

/*
** test_vrshl_u64:
** urshl	d0, d0, d1
** ret
*/
TEST_BINARY (vrshl_u64, uint64x1_t, uint64x1_t, int64x1_t)

/*
** test_vqshlq_s8:
** sqshl	v0\.16b, v0\.16b, v1\.16b
** ret
*/
TEST_UNIFORM_BINARY (vqshlq_s8, int8x16_t)

/*
** test_vqrshl_u32:
** uqrshl	v0\.2s, v0\.2s, v1\.2s
** ret
*/
TEST_BINARY (vqrshl_u32, uint32x2_t, uint32x2_t, int32x2_t)

/*
** test_vqshl_s64:
** sqshl	d0, d0, d1
** ret
*/
TEST_UNIFORM_BINARY (vqshl_s64, int64x1_t)

/*
** test_vshlq_s16_cst:
** shl	v0\.8h, v0\.8h, 3
** ret
*/
TEST_SHIFT_CST (vshlq_s16, int16x8_t, vdupq_n_s16, 3)

/*
** test_vshlq_u16_cst:
** ushr	v0\.8h, v0\.8h, 3
** ret
*/
TEST_SHIFT_CST (vshlq_u16, uint16x8_t, vdupq_n_s16, -3)

/*
** test_vshld_s64:
** fmov	d[0-9]+, x0
** fmov	d[0-9]+, x1
** sshl	d[0-9]+, d[0-9]+, d[0-9]+
** fmov	x0, d[0-9]+
** ret
*/
TEST_BINARY (vshld_s64, int64_t, int64_t, int64_t)

/*
** test_vrshld_u64:
** fmov	d[0-9]+, x0
** fmov	d[0-9]+, x1
** urshl	d[0-9]+, d[0-9]+, d[0-9]+
** fmov	x0, d[0-9]+
** ret
*/
TEST_BINARY (vrshld_u64, uint64_t, uint64_t, int64_t)

/*
** test_vqshlb_s8:
** dup	v[0-9]+\.8b, w0
** dup	v[0-9]+\.8b, w1
** sqshl	b[0-9]+, b[0-9]+, b[0-9]+
** umov	w0, v[0-9]+\.b\[0\]
** ret
*/
TEST_BINARY (vqshlb_s8, int8_t, int8_t, int8_t)

/*
** test_vqrshls_u32:
** fmov	s[0-9]+, w0
** fmov	s[0-9]+, w1
** uqrshl	s[0-9]+, s[0-9]+, s[0-9]+
** fmov	w0, s[0-9]+
** ret
*/
TEST_BINARY (vqrshls_u32, uint32_t, uint32_t, int32_t)
