/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vhadd_u8:
** uhadd	v0\.8b, (v0\.8b, v1\.8b|v1\.8b, v0\.8b)
** ret
*/
TEST_UNIFORM_BINARY (vhadd_u8, uint8x8_t)

/*
** test_vhadd_s8:
** shadd	v0\.8b, (v0\.8b, v1\.8b|v1\.8b, v0\.8b)
** ret
*/
TEST_UNIFORM_BINARY (vhadd_s8, int8x8_t)

/*
** test_vhadd_u16:
** uhadd	v0\.4h, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_UNIFORM_BINARY (vhadd_u16, uint16x4_t)

/*
** test_vhadd_s16:
** shadd	v0\.4h, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_UNIFORM_BINARY (vhadd_s16, int16x4_t)

/*
** test_vhadd_u32:
** uhadd	v0\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_UNIFORM_BINARY (vhadd_u32, uint32x2_t)

/*
** test_vhadd_s32:
** shadd	v0\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_UNIFORM_BINARY (vhadd_s32, int32x2_t)

/*
** test_vhaddq_u8:
** uhadd	v0\.16b, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
** ret
*/
TEST_UNIFORM_BINARY (vhaddq_u8, uint8x16_t)

/*
** test_vhaddq_s8:
** shadd	v0\.16b, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
** ret
*/
TEST_UNIFORM_BINARY (vhaddq_s8, int8x16_t)

/*
** test_vhaddq_u16:
** uhadd	v0\.8h, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_UNIFORM_BINARY (vhaddq_u16, uint16x8_t)

/*
** test_vhaddq_s16:
** shadd	v0\.8h, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_UNIFORM_BINARY (vhaddq_s16, int16x8_t)

/*
** test_vhaddq_u32:
** uhadd	v0\.4s, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_UNIFORM_BINARY (vhaddq_u32, uint32x4_t)

/*
** test_vhaddq_s32:
** shadd	v0\.4s, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_UNIFORM_BINARY (vhaddq_s32, int32x4_t)
