/* { dg-do assemble } */
/* { dg-options "-O --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#pragma GCC target "+nosimd+fp"

#define TEST_VECTOR(TYPE) \
  TYPE \
  test_##TYPE (void) \
  { \
    typedef TYPE v __attribute__((aligned(1))); \
    register v *ptr asm ("x0"); \
    asm volatile ("" : "=r" (ptr)); \
    return *ptr; \
  }

TEST_VECTOR (__Int8x16_t)
TEST_VECTOR (__Int16x8_t)
TEST_VECTOR (__Int32x4_t)
TEST_VECTOR (__Int64x2_t)
TEST_VECTOR (__Bfloat16x8_t)
TEST_VECTOR (__Float16x8_t)
TEST_VECTOR (__Float32x4_t)
TEST_VECTOR (__Float64x2_t)
TEST_VECTOR (__Mfloat8x16_t)

/*
** test___Int8x16_t:
**	ldr	q0, \[x0\]
**	ret
*/
