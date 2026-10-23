/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vqmovn_s16:
** sqxtn	v0\.8b, v0\.8h
** ret
*/
TEST_UNARY (vqmovn_s16, int8x8_t, int16x8_t)

/*
** test_vqmovn_s32:
** sqxtn	v0\.4h, v0\.4s
** ret
*/
TEST_UNARY (vqmovn_s32, int16x4_t, int32x4_t)

/*
** test_vqmovn_s64:
** sqxtn	v0\.2s, v0\.2d
** ret
*/
TEST_UNARY (vqmovn_s64, int32x2_t, int64x2_t)

/*
** test_vqmovn_u16:
** uqxtn	v0\.8b, v0\.8h
** ret
*/
TEST_UNARY (vqmovn_u16, uint8x8_t, uint16x8_t)

/*
** test_vqmovn_u32:
** uqxtn	v0\.4h, v0\.4s
** ret
*/
TEST_UNARY (vqmovn_u32, uint16x4_t, uint32x4_t)

/*
** test_vqmovn_u64:
** uqxtn	v0\.2s, v0\.2d
** ret
*/
TEST_UNARY (vqmovn_u64, uint32x2_t, uint64x2_t)

/*
** test_vqmovnh_s16:
** dup	v31\.4h, w0
** sqxtn	b31, h31
** umov	w0, v31\.b\[0\]
** ret
*/
TEST_UNARY (vqmovnh_s16, int8_t, int16_t)

/*
** test_vqmovns_s32:
** fmov	s31, w0
** sqxtn	h31, s31
** umov	w0, v31\.h\[0\]
** ret
*/
TEST_UNARY (vqmovns_s32, int16_t, int32_t)

/*
** test_vqmovnd_s64:
** fmov	d31, x0
** sqxtn	s31, d31
** fmov	w0, s31
** ret
*/
TEST_UNARY (vqmovnd_s64, int32_t, int64_t)

/*
** test_vqmovnh_u16:
** dup	v31\.4h, w0
** uqxtn	b31, h31
** umov	w0, v31\.b\[0\]
** ret
*/
TEST_UNARY (vqmovnh_u16, uint8_t, uint16_t)

/*
** test_vqmovns_u32:
** fmov	s31, w0
** uqxtn	h31, s31
** umov	w0, v31\.h\[0\]
** ret
*/
TEST_UNARY (vqmovns_u32, uint16_t, uint32_t)

/*
** test_vqmovnd_u64:
** fmov	d31, x0
** uqxtn	s31, d31
** fmov	w0, s31
** ret
*/
TEST_UNARY (vqmovnd_u64, uint32_t, uint64_t)

/*
** test_vqmovn_high_s16:
** sqxtn2	v0\.16b, v1\.8h
** ret
*/
TEST_BINARY (vqmovn_high_s16, int8x16_t, int8x8_t, int16x8_t)

/*
** test_vqmovn_high_s32:
** sqxtn2	v0\.8h, v1\.4s
** ret
*/
TEST_BINARY (vqmovn_high_s32, int16x8_t, int16x4_t, int32x4_t)

/*
** test_vqmovn_high_s64:
** sqxtn2	v0\.4s, v1\.2d
** ret
*/
TEST_BINARY (vqmovn_high_s64, int32x4_t, int32x2_t, int64x2_t)

/*
** test_vqmovn_high_u16:
** uqxtn2	v0\.16b, v1\.8h
** ret
*/
TEST_BINARY (vqmovn_high_u16, uint8x16_t, uint8x8_t, uint16x8_t)

/*
** test_vqmovn_high_u32:
** uqxtn2	v0\.8h, v1\.4s
** ret
*/
TEST_BINARY (vqmovn_high_u32, uint16x8_t, uint16x4_t, uint32x4_t)

/*
** test_vqmovn_high_u64:
** uqxtn2	v0\.4s, v1\.2d
** ret
*/
TEST_BINARY (vqmovn_high_u64, uint32x4_t, uint32x2_t, uint64x2_t)
