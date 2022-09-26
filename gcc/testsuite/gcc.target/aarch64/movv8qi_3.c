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

TEST_VECTOR (__Int8x8_t)
TEST_VECTOR (__Int16x4_t)
TEST_VECTOR (__Int32x2_t)
TEST_VECTOR (__Int64x1_t)
TEST_VECTOR (__Bfloat16x4_t)
TEST_VECTOR (__Float16x4_t)
TEST_VECTOR (__Float32x2_t)
TEST_VECTOR (__Float64x1_t)

/*
** test___Int8x8_t:
**	ldr	d0, \[x0\]
**	ret
*/
