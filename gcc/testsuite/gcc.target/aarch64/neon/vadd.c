/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vadd_u8:
** add	v0\.8b, (v0\.8b, v1\.8b|v1\.8b, v0\.8b)
** ret
*/
TEST_UNIFORM_BINARY (vadd_u8, uint8x8_t)

/*
** test_vadd_s8:
** add	v0\.8b, (v0\.8b, v1\.8b|v1\.8b, v0\.8b)
** ret
*/
TEST_UNIFORM_BINARY (vadd_s8, int8x8_t)

/*
** test_vadd_p8:
** eor	v0\.8b, (v0\.8b, v1\.8b|v1\.8b, v0\.8b)
** ret
*/
TEST_UNIFORM_BINARY (vadd_p8, poly8x8_t)

/*
** test_vadd_u16:
** add	v0\.4h, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_UNIFORM_BINARY (vadd_u16, uint16x4_t)

/*
** test_vadd_s16:
** add	v0\.4h, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_UNIFORM_BINARY (vadd_s16, int16x4_t)

/*
** test_vadd_f16:
** fadd	v0\.4h, (v0\.4h, v1\.4h|v1\.4h, v0\.4h)
** ret
*/
TEST_UNIFORM_BINARY (vadd_f16, float16x4_t)

/*
** test_vadd_p16:
** eor	v0\.8b, (v0\.8b, v1\.8b|v1\.8b, v0\.8b)
** ret
*/
TEST_UNIFORM_BINARY (vadd_p16, poly16x4_t)

/*
** test_vadd_u32:
** add	v0\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_UNIFORM_BINARY (vadd_u32, uint32x2_t)

/*
** test_vadd_s32:
** add	v0\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_UNIFORM_BINARY (vadd_s32, int32x2_t)

/*
** test_vadd_f32:
** fadd	v0\.2s, (v0\.2s, v1\.2s|v1\.2s, v0\.2s)
** ret
*/
TEST_UNIFORM_BINARY (vadd_f32, float32x2_t)

/*
** test_vadd_u64:
** add	d0, (d0, d1|d1, d0)
** ret
*/
TEST_UNIFORM_BINARY (vadd_u64, uint64x1_t)

/*
** test_vadd_s64:
** add	d0, (d0, d1|d1, d0)
** ret
*/
TEST_UNIFORM_BINARY (vadd_s64, int64x1_t)

/*
** test_vadd_f64:
** fadd	d0, (d0, d1|d1, d0)
** ret
*/
TEST_UNIFORM_BINARY (vadd_f64, float64x1_t)

/*
** test_vadd_p64:
** eor	v0\.8b, (v0\.8b, v1\.8b|v1\.8b, v0\.8b)
** ret
*/
TEST_UNIFORM_BINARY (vadd_p64, poly64x1_t)

/*
** test_vaddq_u8:
** add	v0\.16b, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
** ret
*/
TEST_UNIFORM_BINARY (vaddq_u8, uint8x16_t)

/*
** test_vaddq_s8:
** add	v0\.16b, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
** ret
*/
TEST_UNIFORM_BINARY (vaddq_s8, int8x16_t)

/*
** test_vaddq_p8:
** eor	v0\.16b, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
** ret
*/
TEST_UNIFORM_BINARY (vaddq_p8, poly8x16_t)

/*
** test_vaddq_u16:
** add	v0\.8h, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_UNIFORM_BINARY (vaddq_u16, uint16x8_t)

/*
** test_vaddq_s16:
** add	v0\.8h, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_UNIFORM_BINARY (vaddq_s16, int16x8_t)

/*
** test_vaddq_f16:
** fadd	v0\.8h, (v0\.8h, v1\.8h|v1\.8h, v0\.8h)
** ret
*/
TEST_UNIFORM_BINARY (vaddq_f16, float16x8_t)

/*
** test_vaddq_p16:
** eor	v0\.16b, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
** ret
*/
TEST_UNIFORM_BINARY (vaddq_p16, poly16x8_t)

/*
** test_vaddq_u32:
** add	v0\.4s, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_UNIFORM_BINARY (vaddq_u32, uint32x4_t)

/*
** test_vaddq_s32:
** add	v0\.4s, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_UNIFORM_BINARY (vaddq_s32, int32x4_t)

/*
** test_vaddq_f32:
** fadd	v0\.4s, (v0\.4s, v1\.4s|v1\.4s, v0\.4s)
** ret
*/
TEST_UNIFORM_BINARY (vaddq_f32, float32x4_t)

/*
** test_vaddq_u64:
** add	v0\.2d, (v0\.2d, v1\.2d|v1\.2d, v0\.2d)
** ret
*/
TEST_UNIFORM_BINARY (vaddq_u64, uint64x2_t)

/*
** test_vaddq_s64:
** add	v0\.2d, (v0\.2d, v1\.2d|v1\.2d, v0\.2d)
** ret
*/
TEST_UNIFORM_BINARY (vaddq_s64, int64x2_t)

/*
** test_vaddq_f64:
** fadd	v0\.2d, (v0\.2d, v1\.2d|v1\.2d, v0\.2d)
** ret
*/
TEST_UNIFORM_BINARY (vaddq_f64, float64x2_t)

/*
** test_vaddq_p64:
** eor	v0\.16b, (v0\.16b, v1\.16b|v1\.16b, v0\.16b)
** ret
*/
TEST_UNIFORM_BINARY (vaddq_p64, poly64x2_t)

/* `poly128_t` is a scalar type, like `__uint128_t`, so it is passed in two GPR
    registers.  */
/*
** test_vaddq_p128:
** eor	x[0-9], x[0-9]+, x[0-9]+
** eor	x[0-9], x[0-9]+, x[0-9]+
** ret
*/
TEST_UNIFORM_BINARY (vaddq_p128, poly128_t)

/*
** test_vaddd_u64:
** add	x0, (x0, x1|x1, x0)
** ret
*/
TEST_UNIFORM_BINARY (vaddd_u64, uint64_t)

/*
** test_vaddd_s64:
** add	x0, (x0, x1|x1, x0)
** ret
*/
TEST_UNIFORM_BINARY (vaddd_s64, int64_t)
