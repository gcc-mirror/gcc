#include <stdint-gcc.h>

#define TEST(TYPE1, TYPE2)                                        \
  __attribute__ ((noipa))					  \
  void vfcvt_##TYPE1##TYPE2 (TYPE2 *restrict dst,		  \
			     TYPE1 *restrict a, int n)		  \
  {								  \
    for (int i = 0; i < n; i++)					  \
      dst[i] = (TYPE2) a[i];                                      \
  }

#define TEST_ALL()						  \
  TEST (int32_t, float)						  \
  TEST (uint32_t, float)					  \
  TEST (int64_t, double)					  \
  TEST (uint64_t, double)					  \
  TEST (int16_t, _Float16)					  \
  TEST (uint16_t, _Float16)					  \

TEST_ALL ()
