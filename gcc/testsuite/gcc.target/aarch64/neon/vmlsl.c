/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vmlsl_u8:
** umlsl	v0\.8h, (v1\.8b, v2\.8b|v2\.8b, v1\.8b)
** ret
*/
TEST_TERNARY (vmlsl_u8, uint16x8_t, uint16x8_t, uint8x8_t, uint8x8_t)

/*
** test_vmlsl_high_u8:
** umlsl2	v0\.8h, (v1\.16b, v2\.16b|v2\.16b, v1\.16b)
** ret
*/
TEST_TERNARY (vmlsl_high_u8, uint16x8_t, uint16x8_t, uint8x16_t, uint8x16_t)

/*
** test_vmlsl_s8:
** smlsl	v0\.8h, (v1\.8b, v2\.8b|v2\.8b, v1\.8b)
** ret
*/
TEST_TERNARY (vmlsl_s8, int16x8_t, int16x8_t, int8x8_t, int8x8_t)

/*
** test_vmlsl_high_s8:
** smlsl2	v0\.8h, (v1\.16b, v2\.16b|v2\.16b, v1\.16b)
** ret
*/
TEST_TERNARY (vmlsl_high_s8, int16x8_t, int16x8_t, int8x16_t, int8x16_t)

/*
** test_vmlsl_u16:
** umlsl	v0\.4s, (v1\.4h, v2\.4h|v2\.4h, v1\.4h)
** ret
*/
TEST_TERNARY (vmlsl_u16, uint32x4_t, uint32x4_t, uint16x4_t, uint16x4_t)

/*
** test_vmlsl_high_u16:
** umlsl2	v0\.4s, (v1\.8h, v2\.8h|v2\.8h, v1\.8h)
** ret
*/
TEST_TERNARY (vmlsl_high_u16, uint32x4_t, uint32x4_t, uint16x8_t, uint16x8_t)

/*
** test_vmlsl_s16:
** smlsl	v0\.4s, (v1\.4h, v2\.4h|v2\.4h, v1\.4h)
** ret
*/
TEST_TERNARY (vmlsl_s16, int32x4_t, int32x4_t, int16x4_t, int16x4_t)

/*
** test_vmlsl_high_s16:
** smlsl2	v0\.4s, (v1\.8h, v2\.8h|v2\.8h, v1\.8h)
** ret
*/
TEST_TERNARY (vmlsl_high_s16, int32x4_t, int32x4_t, int16x8_t, int16x8_t)

/*
** test_vmlsl_u32:
** umlsl	v0\.2d, (v1\.2s, v2\.2s|v2\.2s, v1\.2s)
** ret
*/
TEST_TERNARY (vmlsl_u32, uint64x2_t, uint64x2_t, uint32x2_t, uint32x2_t)

/*
** test_vmlsl_high_u32:
** umlsl2	v0\.2d, (v1\.4s, v2\.4s|v2\.4s, v1\.4s)
** ret
*/
TEST_TERNARY (vmlsl_high_u32, uint64x2_t, uint64x2_t, uint32x4_t, uint32x4_t)

/*
** test_vmlsl_s32:
** smlsl	v0\.2d, (v1\.2s, v2\.2s|v2\.2s, v1\.2s)
** ret
*/
TEST_TERNARY (vmlsl_s32, int64x2_t, int64x2_t, int32x2_t, int32x2_t)

/*
** test_vmlsl_high_s32:
** smlsl2	v0\.2d, (v1\.4s, v2\.4s|v2\.4s, v1\.4s)
** ret
*/
TEST_TERNARY (vmlsl_high_s32, int64x2_t, int64x2_t, int32x4_t, int32x4_t)
