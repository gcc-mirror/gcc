/* { dg-options "-fpack-struct=2" } */

#include <arm_neon.h>

static_assert(alignof(int32x2_t) == 8, "int32x2_t alignment");
static_assert(alignof(int32x4_t) == 16, "int32x4_t alignment");
static_assert(alignof(int32x2x2_t) == 2, "int32x2x2_t alignment");
static_assert(alignof(int32x4x2_t) == 2, "int32x4x2_t alignment");
static_assert(alignof(int32x2x3_t) == 2, "int32x2x3_t alignment");
static_assert(alignof(int32x4x3_t) == 2, "int32x4x3_t alignment");
static_assert(alignof(int32x2x4_t) == 2, "int32x2x4_t alignment");
static_assert(alignof(int32x4x4_t) == 2, "int32x4x4_t alignment");
