/* { dg-do assemble } */
/* { dg-options "-O --save-temps" } */

#pragma GCC target "+nosimd+fp"

#define TEST_GENERAL(TYPE) \
  TYPE mov_##TYPE (TYPE a, TYPE b) { return b; } \
  TYPE zero_##TYPE () { return (TYPE) {}; } \
  TYPE load_##TYPE (TYPE *ptr) { return *ptr; } \
  void store_##TYPE (TYPE *ptr, TYPE a) { *ptr = a; }

TEST_GENERAL (__Int8x8_t)
TEST_GENERAL (__Int16x4_t)
TEST_GENERAL (__Int32x2_t)
TEST_GENERAL (__Int64x1_t)
TEST_GENERAL (__Bfloat16x4_t)
TEST_GENERAL (__Float16x4_t)
TEST_GENERAL (__Float32x2_t)
TEST_GENERAL (__Float64x1_t)

__Int8x8_t const_s8x8 () { return (__Int8x8_t) { 1, 1, 1, 1, 1, 1, 1, 1 }; }
__Int16x4_t const_s16x4 () { return (__Int16x4_t) { 1, 0, 1, 0 }; }
__Int32x2_t const_s32x2 () { return (__Int32x2_t) { 1, 2 }; }
__Int64x1_t const_s64x1 () { return (__Int64x1_t) { 100 }; }
__Float16x4_t const_f16x4 () { return (__Float16x4_t) { 2, 2, 2, 2 }; }
__Float32x2_t const_f32x2 () { return (__Float32x2_t) { 1, 2 }; }
__Float64x1_t const_f64x1 () { return (__Float64x1_t) { 32 }; }
