#include <stdint-gcc.h>

#define TEST(TYPE1, TYPE2)                                                     \
  __attribute__ ((noipa)) void vfwcvt_##TYPE1##TYPE2 (TYPE2 *dst, TYPE1 *a,    \
						     int n)                    \
  {                                                                            \
    for (int i = 0; i < n; i++)                                                \
      dst[i] = (TYPE2) a[i];                                                   \
  }

#define TEST_ALL()                                                             \
  TEST (_Float16, int64_t)                                                     \
  TEST (_Float16, uint64_t)                                                    \
  TEST (_Float16, int32_t)                                                     \
  TEST (_Float16, uint32_t)                                                    \
  TEST (float, int64_t)							       \
  TEST (float, uint64_t)						       \

TEST_ALL ()
