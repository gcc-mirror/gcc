#include <stdint-gcc.h>

#define TEST(TYPE1, TYPE2)                                                     \
  __attribute__ ((noipa)) void vfncvt_##TYPE1##TYPE2 (TYPE2 *dst, TYPE1 *a,    \
						     int n)                    \
  {                                                                            \
    for (int i = 0; i < n; i++)                                                \
      dst[i] = (TYPE2) a[i];                                                   \
  }

#define TEST_ALL()                                                             \
  TEST (double, int32_t)                                                       \
  TEST (double, uint32_t)                                                      \
  TEST (double, int16_t)                                                       \
  TEST (double, uint16_t)                                                      \
  TEST (double, int8_t)                                                        \
  TEST (double, uint8_t)                                                       \
  TEST (float, int16_t)							       \
  TEST (float, uint16_t)						       \
  TEST (float, int8_t)							       \
  TEST (float, uint8_t)							       \
  TEST (_Float16, int8_t)						       \
  TEST (_Float16, uint8_t)						       \

TEST_ALL ()
