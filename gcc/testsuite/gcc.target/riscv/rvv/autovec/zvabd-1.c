/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv_zvabd -mabi=lp64d -fno-vect-cost-model" } */

#include <stdint-gcc.h>

static int64_t
abs64 (int64_t i)
{
  return i < 0 ? -i : i;
}

#define TEST_VABS(TYPE1, TYPE2)                                            \
  __attribute__((noipa)) void vabs_##TYPE1_##TYPE2 (TYPE1 *__restrict dst, \
						    TYPE2 *__restrict a,   \
						    int n)                 \
  {                                                                        \
    int i;                                                                 \
    for (i = 0; i < n; i++)                                                \
      dst[i] = abs64 (a[i]);                                               \
  }

#define TEST_VABD(TYPE1, TYPE2)                                            \
  __attribute__((noipa)) void vabd_##TYPE1_##TYPE2 (TYPE1 *__restrict dst, \
						    TYPE2 *__restrict a,   \
						    TYPE2 *__restrict b,   \
						    int n)                 \
  {                                                                        \
    int i;                                                                 \
    for (i = 0; i < n; i++)                                                \
      dst[i] = abs64 ((int64_t) a[i] - (int64_t) b[i]);                    \
  }

#define TEST_VWABDA(TYPE1, TYPE2)                                       \
  __attribute__((noipa)) void                                           \
  vwabda_##TYPE1_##TYPE2 (TYPE1 *__restrict dst, TYPE2 *__restrict a,   \
			  TYPE2 *__restrict b, int n)                   \
  {                                                                     \
    int i;                                                              \
    for (i = 0; i < n; i++)                                             \
      dst[i] += abs64 ((int64_t) a[i] - (int64_t) b[i]);                \
  }

#define TEST_ALL()                 \
  TEST_VABS (int8_t, int8_t)       \
  TEST_VABS (int16_t, int16_t)     \
  TEST_VABD (int8_t, int8_t)       \
  TEST_VABD (uint8_t, uint8_t)     \
  TEST_VABD (int16_t, int16_t)     \
  TEST_VABD (uint16_t, uint16_t)   \
  TEST_VABD (int32_t, int32_t)     \
  TEST_VWABDA (int16_t, int8_t)    \
  TEST_VWABDA (uint16_t, uint8_t)  \
  TEST_VWABDA (int32_t, int16_t)   \
  TEST_VWABDA (uint32_t, uint16_t) \
  TEST_VWABDA (int64_t, int32_t)   \
  TEST_VWABDA (uint64_t, uint32_t)

TEST_ALL()

/* { dg-final { scan-assembler-times {\tvabs\.v} 5 } } */
/* { dg-final { scan-assembler-times {\tvabd\.vv} 2 } } */
/* { dg-final { scan-assembler-times {\tvabdu\.vv} 2 } } */
/* { dg-final { scan-assembler-times {\tvwabda\.vv} 2 } } */
/* { dg-final { scan-assembler-times {\tvwabdau\.vv} 2 } } */
