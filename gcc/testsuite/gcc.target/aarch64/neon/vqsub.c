/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vqsub_u8:
** uqsub	v0\.8b, v0\.8b, v1\.8b
** ret
*/
TEST_UNIFORM_BINARY (vqsub_u8, uint8x8_t)

/*
** test_vqsub_s8:
** sqsub	v0\.8b, v0\.8b, v1\.8b
** ret
*/
TEST_UNIFORM_BINARY (vqsub_s8, int8x8_t)

/*
** test_vqsub_u16:
** uqsub	v0\.4h, v0\.4h, v1\.4h
** ret
*/
TEST_UNIFORM_BINARY (vqsub_u16, uint16x4_t)

/*
** test_vqsub_s16:
** sqsub	v0\.4h, v0\.4h, v1\.4h
** ret
*/
TEST_UNIFORM_BINARY (vqsub_s16, int16x4_t)

/*
** test_vqsub_u32:
** uqsub	v0\.2s, v0\.2s, v1\.2s
** ret
*/
TEST_UNIFORM_BINARY (vqsub_u32, uint32x2_t)

/*
** test_vqsub_s32:
** sqsub	v0\.2s, v0\.2s, v1\.2s
** ret
*/
TEST_UNIFORM_BINARY (vqsub_s32, int32x2_t)

/*
** test_vqsub_u64:
** uqsub	d0, d0, d1
** ret
*/
TEST_UNIFORM_BINARY (vqsub_u64, uint64x1_t)

/*
** test_vqsub_s64:
** sqsub	d0, d0, d1
** ret
*/
TEST_UNIFORM_BINARY (vqsub_s64, int64x1_t)

/*
** test_vqsubq_u8:
** uqsub	v0\.16b, v0\.16b, v1\.16b
** ret
*/
TEST_UNIFORM_BINARY (vqsubq_u8, uint8x16_t)

/*
** test_vqsubq_s8:
** sqsub	v0\.16b, v0\.16b, v1\.16b
** ret
*/
TEST_UNIFORM_BINARY (vqsubq_s8, int8x16_t)

/*
** test_vqsubq_u16:
** uqsub	v0\.8h, v0\.8h, v1\.8h
** ret
*/
TEST_UNIFORM_BINARY (vqsubq_u16, uint16x8_t)

/*
** test_vqsubq_s16:
** sqsub	v0\.8h, v0\.8h, v1\.8h
** ret
*/
TEST_UNIFORM_BINARY (vqsubq_s16, int16x8_t)

/*
** test_vqsubq_u32:
** uqsub	v0\.4s, v0\.4s, v1\.4s
** ret
*/
TEST_UNIFORM_BINARY (vqsubq_u32, uint32x4_t)

/*
** test_vqsubq_s32:
** sqsub	v0\.4s, v0\.4s, v1\.4s
** ret
*/
TEST_UNIFORM_BINARY (vqsubq_s32, int32x4_t)

/*
** test_vqsubq_u64:
** uqsub	v0\.2d, v0\.2d, v1\.2d
** ret
*/
TEST_UNIFORM_BINARY (vqsubq_u64, uint64x2_t)

/*
** test_vqsubq_s64:
** sqsub	v0\.2d, v0\.2d, v1\.2d
** ret
*/
TEST_UNIFORM_BINARY (vqsubq_s64, int64x2_t)

/*
** test_vqsubb_u8:
** dup	v([0-9]+)\.8b, w0
** dup	v([0-9]+)\.8b, w1
** uqsub	b([0-9]+), b\1, b\2
** umov	w0, v\3\.b\[0\]
** ret
*/
TEST_UNIFORM_BINARY (vqsubb_u8, uint8_t)

/*
** test_vqsubb_s8:
** dup	v([0-9]+)\.8b, w0
** dup	v([0-9]+)\.8b, w1
** sqsub	b([0-9]+), b\1, b\2
** umov	w0, v\3\.b\[0\]
** ret
*/
TEST_UNIFORM_BINARY (vqsubb_s8, int8_t)

/*
** test_vqsubh_u16:
** dup	v([0-9]+)\.4h, w0
** dup	v([0-9]+)\.4h, w1
** uqsub	h([0-9]+), h\1, h\2
** umov	w0, v\3\.h\[0\]
** ret
*/
TEST_UNIFORM_BINARY (vqsubh_u16, uint16_t)

/*
** test_vqsubh_s16:
** dup	v([0-9]+)\.4h, w0
** dup	v([0-9]+)\.4h, w1
** sqsub	h([0-9]+), h\1, h\2
** umov	w0, v\3\.h\[0\]
** ret
*/
TEST_UNIFORM_BINARY (vqsubh_s16, int16_t)

/*
** test_vqsubs_u32:
** subs	(w[0-9]+), w0, w1
** csel	w0, \1, wzr, cs
** ret
*/
TEST_UNIFORM_BINARY (vqsubs_u32, uint32_t)

/*
** test_vqsubs_s32:
** fmov	(s[0-9]+), w0
** fmov	(s[0-9]+), w1
** sqsub	s([0-9]+), \1, \2
** fmov	w0, s\3
** ret
*/
TEST_UNIFORM_BINARY (vqsubs_s32, int32_t)

/*
** test_vqsubd_u64:
** subs	(x[0-9]+), x0, x1
** csel	x0, \1, xzr, cs
** ret
*/
TEST_UNIFORM_BINARY (vqsubd_u64, uint64_t)

/*
** test_vqsubd_s64:
** fmov	(d[0-9]+), x0
** fmov	(d[0-9]+), x1
** sqsub	d([0-9]+), \1, \2
** fmov	x0, d\3
** ret
*/
TEST_UNIFORM_BINARY (vqsubd_s64, int64_t)
