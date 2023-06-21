#include <stdint-gcc.h>

#define TEST(TYPE1, TYPE2)                                                     \
  __attribute__ ((noipa)) void vfcvt_##TYPE1##TYPE2 (TYPE2 *dst, TYPE1 *a,     \
						     int n)                    \
  {                                                                            \
    for (int i = 0; i < n; i++)                                                \
      dst[i] = (TYPE1) a[i];                                                   \
  }

#define TEST_ALL()                                                             \
  TEST (float, int32_t)                                                        \
  TEST (double, int64_t)

TEST_ALL ()
