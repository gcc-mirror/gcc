#include <stdint-gcc.h>

#define TEST_TYPE(TYPE, BITS)                                                  \
  __attribute__ ((noipa)) void vbswap_##TYPE (TYPE *restrict dst,              \
					      TYPE *restrict a, int n)         \
  {                                                                            \
    for (int i = 0; i < n; i++)                                                \
      dst[i] = __builtin_bswap##BITS (a[i]);                                   \
  }

#define TEST_ALL()                                                             \
  TEST_TYPE (uint16_t, 16)                                                     \
  TEST_TYPE (uint32_t, 32)                                                     \
  TEST_TYPE (uint64_t, 64)

TEST_ALL ()
