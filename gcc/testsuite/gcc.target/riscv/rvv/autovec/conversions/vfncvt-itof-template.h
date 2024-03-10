#include <stdint-gcc.h>

#define TEST(TYPE1, TYPE2)                                        \
  __attribute__ ((noipa))					  \
  void vfncvt_##TYPE1##TYPE2 (TYPE2 *restrict dst,		  \
			      TYPE1 *restrict a, int n)		  \
  {								  \
    for (int i = 0; i < n; i++)					  \
      dst[i] = (TYPE2) a[i];                                      \
  }

#define TEST_ALL()						  \
  TEST (int64_t, float)						  \
  TEST (uint64_t, float)					  \
  TEST (int32_t, _Float16)					  \
  TEST (uint32_t, _Float16)					  \

TEST_ALL ()
