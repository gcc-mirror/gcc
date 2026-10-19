/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vrhadd_u8:
** urhadd	v0\.8b, (v0\.8b, v1\.8b|v1\.8b, v0\.8b)
** ret
*/
TEST_UNIFORM_BINARY (vrhadd_u8, uint8x8_t)

/*
** test_vrhadd_s8:
** srhadd	v0\.8b, (v0\.8b, v1\.8b|v1\.8b, v0\.8b)
** ret
*/
TEST_UNIFORM_BINARY (vrhadd_s8, int8x8_t)

/*
** test_vrhadd_u16:
** urhadd	v0\.4h, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_UNIFORM_BINARY (vrhadd_u16, uint16x4_t)

/*
** test_vrhadd_s16:
** srhadd	v0\.4h, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_UNIFORM_BINARY (vrhadd_s16, int16x4_t)

/*
** test_vrhadd_u32:
** urhadd	v0\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_UNIFORM_BINARY (vrhadd_u32, uint32x2_t)

/*
** test_vrhadd_s32:
** srhadd	v0\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_UNIFORM_BINARY (vrhadd_s32, int32x2_t)

/*
** test_vrhaddq_u8:
** urhadd	v0\.16b, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
** ret
*/
TEST_UNIFORM_BINARY (vrhaddq_u8, uint8x16_t)

/*
** test_vrhaddq_s8:
** srhadd	v0\.16b, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
** ret
*/
TEST_UNIFORM_BINARY (vrhaddq_s8, int8x16_t)

/*
** test_vrhaddq_u16:
** urhadd	v0\.8h, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_UNIFORM_BINARY (vrhaddq_u16, uint16x8_t)

/*
** test_vrhaddq_s16:
** srhadd	v0\.8h, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_UNIFORM_BINARY (vrhaddq_s16, int16x8_t)

/*
** test_vrhaddq_u32:
** urhadd	v0\.4s, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_UNIFORM_BINARY (vrhaddq_u32, uint32x4_t)

/*
** test_vrhaddq_s32:
** srhadd	v0\.4s, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_UNIFORM_BINARY (vrhaddq_s32, int32x4_t)
