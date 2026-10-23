/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vqmovun_s16:
** sqxtun	v0\.8b, v0\.8h
** ret
*/
TEST_UNARY (vqmovun_s16, uint8x8_t, int16x8_t)

/*
** test_vqmovun_s32:
** sqxtun	v0\.4h, v0\.4s
** ret
*/
TEST_UNARY (vqmovun_s32, uint16x4_t, int32x4_t)

/*
** test_vqmovun_s64:
** sqxtun	v0\.2s, v0\.2d
** ret
*/
TEST_UNARY (vqmovun_s64, uint32x2_t, int64x2_t)

/*
** test_vqmovunh_s16:
** dup	v31\.4h, w0
** sqxtun	b31, h31
** umov	w0, v31\.b\[0\]
** ret
*/
TEST_UNARY (vqmovunh_s16, uint8_t, int16_t)

/*
** test_vqmovuns_s32:
** fmov	s31, w0
** sqxtun	h31, s31
** umov	w0, v31\.h\[0\]
** ret
*/
TEST_UNARY (vqmovuns_s32, uint16_t, int32_t)

/*
** test_vqmovund_s64:
** fmov	d31, x0
** sqxtun	s31, d31
** fmov	w0, s31
** ret
*/
TEST_UNARY (vqmovund_s64, uint32_t, int64_t)

/*
** test_vqmovun_high_s16:
** sqxtun2	v0\.16b, v1\.8h
** ret
*/
TEST_BINARY (vqmovun_high_s16, uint8x16_t, uint8x8_t, int16x8_t)

/*
** test_vqmovun_high_s32:
** sqxtun2	v0\.8h, v1\.4s
** ret
*/
TEST_BINARY (vqmovun_high_s32, uint16x8_t, uint16x4_t, int32x4_t)

/*
** test_vqmovun_high_s64:
** sqxtun2	v0\.4s, v1\.2d
** ret
*/
TEST_BINARY (vqmovun_high_s64, uint32x4_t, uint32x2_t, int64x2_t)
