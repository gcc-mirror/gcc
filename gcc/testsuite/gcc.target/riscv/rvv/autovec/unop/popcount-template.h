#include <stdint-gcc.h>

#define TEST_TYPE(TYPE)                                                        \
  __attribute__ ((noipa)) void vpopcount_##TYPE (TYPE *restrict dst,           \
						 TYPE *restrict a, int n)      \
  {                                                                            \
    for (int i = 0; i < n; i++)                                                \
      dst[i] = __builtin_popcount (a[i]);                                      \
  }

#define TEST_ALL()                                                             \
  TEST_TYPE (int8_t)                                                           \
  TEST_TYPE (uint8_t)                                                          \
  TEST_TYPE (int16_t)                                                          \
  TEST_TYPE (uint16_t)                                                         \
  TEST_TYPE (int32_t)                                                          \
  TEST_TYPE (uint32_t)                                                         \
  TEST_TYPE (int64_t)                                                          \
  TEST_TYPE (uint64_t)

TEST_ALL()
