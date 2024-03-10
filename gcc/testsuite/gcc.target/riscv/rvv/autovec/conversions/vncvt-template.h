#include <stdint-gcc.h>

#define TEST(TYPE1, TYPE2)					\
  __attribute__((noipa))					\
  void vncvt_##TYPE1##TYPE2 (TYPE2 *dst, TYPE1 *a, int n)	\
  {								\
    for (int i = 0; i < n; i++)					\
      dst[i] = (TYPE2)a[i];					\
  }

#define TEST_ALL()						\
 TEST(uint16_t, uint8_t)					\
 TEST(uint32_t, uint8_t)					\
 TEST(uint32_t, uint16_t)					\
 TEST(uint64_t, uint8_t)					\
 TEST(uint64_t, uint16_t)					\
 TEST(uint64_t, uint32_t)					\

TEST_ALL()
