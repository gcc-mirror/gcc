#include <stdint-gcc.h>

#define TEST(TYPE1, TYPE2)                                        \
  __attribute__ ((noipa))					  \
  void vfwcvt_##TYPE1##TYPE2 (TYPE2 *restrict dst,		  \
			      TYPE1 *restrict a, int n)		  \
  {								  \
    for (int i = 0; i < n; i++)					  \
      dst[i] = (TYPE2) a[i];                                      \
  }

#define TEST_ALL()						  \
  TEST (int16_t, float)						  \
  TEST (uint16_t, float)					  \
  TEST (int32_t, double)					  \
  TEST (uint32_t, double)					  \
  TEST (int8_t, _Float16)					  \
  TEST (uint8_t, _Float16)					  \

TEST_ALL ()
