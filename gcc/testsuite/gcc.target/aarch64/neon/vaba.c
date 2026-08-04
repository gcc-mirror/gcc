/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vaba_s8:
** saba	v0\.8b, v1\.8b, v2\.8b
** ret
*/
TEST_UNIFORM_TERNARY (vaba_s8, int8x8_t)

/*
** test_vabaq_s8:
** saba	v0\.16b, v1\.16b, v2\.16b
** ret
*/
TEST_UNIFORM_TERNARY (vabaq_s8, int8x16_t)

/*
** test_vaba_s16:
** saba	v0\.4h, v1\.4h, v2\.4h
** ret
*/
TEST_UNIFORM_TERNARY (vaba_s16, int16x4_t)

/*
** test_vabaq_s16:
** saba	v0\.8h, v1\.8h, v2\.8h
** ret
*/
TEST_UNIFORM_TERNARY (vabaq_s16, int16x8_t)

/*
** test_vaba_s32:
** saba	v0\.2s, v1\.2s, v2\.2s
** ret
*/
TEST_UNIFORM_TERNARY (vaba_s32, int32x2_t)

/*
** test_vabaq_s32:
** saba	v0\.4s, v1\.4s, v2\.4s
** ret
*/
TEST_UNIFORM_TERNARY (vabaq_s32, int32x4_t)

/*
** test_vaba_u8:
** uaba	v0\.8b, v1\.8b, v2\.8b
** ret
*/
TEST_UNIFORM_TERNARY (vaba_u8, uint8x8_t)

/*
** test_vabaq_u8:
** uaba	v0\.16b, v1\.16b, v2\.16b
** ret
*/
TEST_UNIFORM_TERNARY (vabaq_u8, uint8x16_t)

/*
** test_vaba_u16:
** uaba	v0\.4h, v1\.4h, v2\.4h
** ret
*/
TEST_UNIFORM_TERNARY (vaba_u16, uint16x4_t)

/*
** test_vabaq_u16:
** uaba	v0\.8h, v1\.8h, v2\.8h
** ret
*/
TEST_UNIFORM_TERNARY (vabaq_u16, uint16x8_t)

/*
** test_vaba_u32:
** uaba	v0\.2s, v1\.2s, v2\.2s
** ret
*/
TEST_UNIFORM_TERNARY (vaba_u32, uint32x2_t)

/*
** test_vabaq_u32:
** uaba	v0\.4s, v1\.4s, v2\.4s
** ret
*/
TEST_UNIFORM_TERNARY (vabaq_u32, uint32x4_t)
