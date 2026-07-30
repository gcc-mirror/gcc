/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vneg_f32:
** fneg	v0\.2s, v0\.2s
** ret
*/
TEST_UNIFORM_UNARY (vneg_f32, float32x2_t)

/*
** test_vneg_f64:
** fneg	d0, d0
** ret
*/
TEST_UNIFORM_UNARY (vneg_f64, float64x1_t)

/*
** test_vneg_s8:
** neg	v0\.8b, v0\.8b
** ret
*/
TEST_UNIFORM_UNARY (vneg_s8, int8x8_t)

/*
** test_vneg_s16:
** neg	v0\.4h, v0\.4h
** ret
*/
TEST_UNIFORM_UNARY (vneg_s16, int16x4_t)

/*
** test_vneg_s32:
** neg	v0\.2s, v0\.2s
** ret
*/
TEST_UNIFORM_UNARY (vneg_s32, int32x2_t)

/*
** test_vneg_s64:
** neg	d0, d0
** ret
*/
TEST_UNIFORM_UNARY (vneg_s64, int64x1_t)

/*
** test_vnegd_s64:
** neg	x0, x0
** ret
*/
TEST_UNIFORM_UNARY (vnegd_s64, int64_t)

/*
** test_vnegq_f32:
** fneg	v0\.4s, v0\.4s
** ret
*/
TEST_UNIFORM_UNARY (vnegq_f32, float32x4_t)

/*
** test_vnegq_f64:
** fneg	v0\.2d, v0\.2d
** ret
*/
TEST_UNIFORM_UNARY (vnegq_f64, float64x2_t)

/*
** test_vnegq_s8:
** neg	v0\.16b, v0\.16b
** ret
*/
TEST_UNIFORM_UNARY (vnegq_s8, int8x16_t)

/*
** test_vnegq_s16:
** neg	v0\.8h, v0\.8h
** ret
*/
TEST_UNIFORM_UNARY (vnegq_s16, int16x8_t)

/*
** test_vnegq_s32:
** neg	v0\.4s, v0\.4s
** ret
*/
TEST_UNIFORM_UNARY (vnegq_s32, int32x4_t)

/*
** test_vnegq_s64:
** neg	v0\.2d, v0\.2d
** ret
*/
TEST_UNIFORM_UNARY (vnegq_s64, int64x2_t)

/*
** test_vneg_f16:
** fneg	v0\.4h, v0\.4h
** ret
*/
TEST_UNIFORM_UNARY (vneg_f16, float16x4_t)

/*
** test_vnegq_f16:
** fneg	v0\.8h, v0\.8h
** ret
*/
TEST_UNIFORM_UNARY (vnegq_f16, float16x8_t)
