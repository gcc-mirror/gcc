/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vqadd_u8:
** uqadd	v0\.8b, (v0\.8b, v1\.8b|v1\.8b, v0\.8b)
** ret
*/
TEST_UNIFORM_BINARY (vqadd_u8, uint8x8_t)

/*
** test_vqadd_s8:
** sqadd	v0\.8b, (v0\.8b, v1\.8b|v1\.8b, v0\.8b)
** ret
*/
TEST_UNIFORM_BINARY (vqadd_s8, int8x8_t)

/*
** test_vqadd_u16:
** uqadd	v0\.4h, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_UNIFORM_BINARY (vqadd_u16, uint16x4_t)

/*
** test_vqadd_s16:
** sqadd	v0\.4h, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_UNIFORM_BINARY (vqadd_s16, int16x4_t)

/*
** test_vqadd_u32:
** uqadd	v0\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_UNIFORM_BINARY (vqadd_u32, uint32x2_t)

/*
** test_vqadd_s32:
** sqadd	v0\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_UNIFORM_BINARY (vqadd_s32, int32x2_t)

/*
** test_vqadd_u64:
** uqadd	d0, (d0, d1|d1, d0)
** ret
*/
TEST_UNIFORM_BINARY (vqadd_u64, uint64x1_t)

/*
** test_vqadd_s64:
** sqadd	d0, (d0, d1|d1, d0)
** ret
*/
TEST_UNIFORM_BINARY (vqadd_s64, int64x1_t)

/*
** test_vqaddq_u8:
** uqadd	v0\.16b, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
** ret
*/
TEST_UNIFORM_BINARY (vqaddq_u8, uint8x16_t)

/*
** test_vqaddq_s8:
** sqadd	v0\.16b, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
** ret
*/
TEST_UNIFORM_BINARY (vqaddq_s8, int8x16_t)

/*
** test_vqaddq_u16:
** uqadd	v0\.8h, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_UNIFORM_BINARY (vqaddq_u16, uint16x8_t)

/*
** test_vqaddq_s16:
** sqadd	v0\.8h, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_UNIFORM_BINARY (vqaddq_s16, int16x8_t)

/*
** test_vqaddq_u32:
** uqadd	v0\.4s, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_UNIFORM_BINARY (vqaddq_u32, uint32x4_t)

/*
** test_vqaddq_s32:
** sqadd	v0\.4s, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_UNIFORM_BINARY (vqaddq_s32, int32x4_t)

/*
** test_vqaddq_u64:
** uqadd	v0\.2d, (v0\.2d, v1\.2d|v1\.2d, v0\.2d)
** ret
*/
TEST_UNIFORM_BINARY (vqaddq_u64, uint64x2_t)

/*
** test_vqaddq_s64:
** sqadd	v0\.2d, (v0\.2d, v1\.2d|v1\.2d, v0\.2d)
** ret
*/
TEST_UNIFORM_BINARY (vqaddq_s64, int64x2_t)

/*
** test_vqaddb_u8:
** dup	v([0-9]+)\.8b, w[0-9]+
** dup	v([0-9]+)\.8b, w[0-9]+
** uqadd	b([0-9]+), (b\2, b\1|b\1, b\2)
** umov	w0, v\3\.b\[0\]
** ret
*/
TEST_UNIFORM_BINARY (vqaddb_u8, uint8_t)

/*
** test_vqaddb_s8:
** dup	v([0-9]+)\.8b, w[0-9]+
** dup	v([0-9]+)\.8b, w[0-9]+
** sqadd	b([0-9]+), (b\2, b\1|b\1, b\2)
** umov	w0, v\3\.b\[0\]
** ret
*/
TEST_UNIFORM_BINARY (vqaddb_s8, int8_t)

/*
** test_vqaddh_u16:
** dup	v([0-9]+)\.4h, w[0-9]+
** dup	v([0-9]+)\.4h, w[0-9]+
** uqadd	h([0-9]+), (h\2, h\1|h\1, h\2)
** umov	w0, v\3\.h\[0\]
** ret
*/
TEST_UNIFORM_BINARY (vqaddh_u16, uint16_t)

/*
** test_vqaddh_s16:
** dup	v([0-9]+)\.4h, w[0-9]+
** dup	v([0-9]+)\.4h, w[0-9]+
** sqadd	h([0-9]+), (h\2, h\1|h\1, h\2)
** umov	w0, v\3\.h\[0\]
** ret
*/
TEST_UNIFORM_BINARY (vqaddh_s16, int16_t)

/*
** test_vqadds_u32:
** adds	(w[0-9]+), (w0, w1|w1, w0)
** csinv	w0, \1, wzr, cc
** ret
*/
TEST_UNIFORM_BINARY (vqadds_u32, uint32_t)

/*
** test_vqadds_s32:
** fmov	(s[0-9]+), w[0-9]+
** fmov	(s[0-9]+), w[0-9]+
** sqadd	s([0-9]+), (\2, \1|\1, \2)
** fmov	w0, s\3
** ret
*/
TEST_UNIFORM_BINARY (vqadds_s32, int32_t)

/*
** test_vqaddd_u64:
** adds	(x[0-9]+), (x0, x1|x1, x0)
** csinv	x0, \1, xzr, cc
** ret
*/
TEST_UNIFORM_BINARY (vqaddd_u64, uint64_t)

/*
** test_vqaddd_s64:
** fmov	(d[0-9]+), x[0-9]+
** fmov	(d[0-9]+), x[0-9]+
** sqadd	d([0-9]+), (\2, \1|\1, \2)
** fmov	x0, d\3
** ret
*/
TEST_UNIFORM_BINARY (vqaddd_s64, int64_t)
