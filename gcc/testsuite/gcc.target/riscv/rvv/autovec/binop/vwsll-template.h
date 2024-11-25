/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-add-options "riscv_v" } */
/* { dg-add-options "riscv_zvbb" } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model" } */

#include <stdint-gcc.h>

#define TEST1_TYPE(TYPE1, TYPE2)                                               \
  __attribute__ ((noipa)) void vwsll_vv##TYPE1 (TYPE1 *restrict dst,           \
						TYPE2 *restrict a,             \
						TYPE2 *restrict b, int n)      \
  {                                                                            \
    for (int i = 0; i < n; i++)                                                \
      dst[i] = (TYPE1) a[i] << b[i];                                           \
  }

#define TEST2_TYPE(TYPE1, TYPE2)                                               \
  __attribute__ ((noipa)) void vwsll_vx##TYPE1 (TYPE1 *restrict dst,           \
						TYPE2 *restrict a, TYPE2 b,    \
						int n)                         \
  {                                                                            \
    for (int i = 0; i < n; i++)                                                \
      dst[i] = (TYPE1) a[i] << b;                                              \
  }

#define TEST3_TYPE(TYPE1, TYPE2)                                               \
  __attribute__ ((noipa)) void vwsll_vi##TYPE1 (TYPE1 *restrict dst,           \
						TYPE2 *restrict a, int n)      \
  {                                                                            \
    for (int i = 0; i < n; i++)                                                \
      dst[i] = (TYPE1) a[i] << 6;                                             \
  }

#define TEST_ALL()                                                             \
  TEST1_TYPE (uint16_t, uint8_t)                                               \
  TEST1_TYPE (uint32_t, uint16_t)                                              \
  TEST1_TYPE (uint64_t, uint32_t)                                              \
  TEST2_TYPE (uint16_t, uint8_t)                                               \
  TEST2_TYPE (uint32_t, uint16_t)                                              \
  TEST2_TYPE (uint64_t, uint32_t)                                              \
  TEST3_TYPE (uint16_t, uint8_t)                                               \
  TEST3_TYPE (uint32_t, uint16_t)                                              \
  TEST3_TYPE (uint64_t, uint32_t)

TEST_ALL()

/* { dg-final { scan-assembler-times {\tvwsll\.vv} 3 } } */
/* { dg-final { scan-assembler-times {\tvwsll\.vx} 3 } } */
/* { dg-final { scan-assembler-times {\tvwsll\.vi} 3 } } */
