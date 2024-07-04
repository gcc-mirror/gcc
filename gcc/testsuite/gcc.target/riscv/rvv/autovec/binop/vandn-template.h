#include <stdint-gcc.h>

#define TEST_TYPE(TYPE)                                                        \
  __attribute__ ((noipa)) void vandn_##TYPE (TYPE *restrict dst,               \
					     TYPE *restrict a,                 \
					     TYPE *restrict b, int n)          \
  {                                                                            \
    for (int i = 0; i < n; i++)                                                \
      dst[i] = a[i] & ~b[i];                                                   \
  }

#define TEST2_TYPE(TYPE)                                                       \
  __attribute__ ((noipa)) void vandns_##TYPE (TYPE *restrict dst,              \
					      TYPE *restrict a, TYPE b, int n) \
  {                                                                            \
    for (int i = 0; i < n; i++)                                                \
      dst[i] = a[i] & ~b;                                                      \
  }

#define TEST_ALL()                                                             \
 TEST_TYPE (int8_t)                                                           \
 TEST_TYPE(uint8_t)	\
 TEST_TYPE(int16_t)	\
 TEST_TYPE(uint16_t)	\
 TEST_TYPE(int32_t)	\
 TEST_TYPE(uint32_t)	\
 TEST_TYPE(int64_t)	\
 TEST_TYPE(uint64_t)    \
 TEST2_TYPE(int8_t)	\
 TEST2_TYPE(uint8_t)	\
 TEST2_TYPE(int16_t)	\
 TEST2_TYPE(uint16_t)	\
 TEST2_TYPE(int32_t)	\
 TEST2_TYPE(uint32_t)	\
 TEST2_TYPE(int64_t)	\
 TEST2_TYPE(uint64_t)

TEST_ALL()
