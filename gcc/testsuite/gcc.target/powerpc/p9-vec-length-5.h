#include "p9-vec-length.h"

/* Test the case that the loop which has multiple vectors (concatenated vectors)
   with different types.  */

#define test(TYPE1, TYPE2)                                                     \
  void __attribute__ ((noinline, noclone))                                     \
    test_mv_##TYPE1##TYPE2 (TYPE1 *restrict a, TYPE2 *restrict b, int n)       \
  {                                                                            \
    for (int i = 0; i < n; ++i)                                                \
      {                                                                        \
	a[i * 2] += 1;                                                         \
	a[i * 2 + 1] += 2;                                                     \
	b[i * 2] += 3;                                                         \
	b[i * 2 + 1] += 4;                                                     \
      }                                                                        \
  }

#define TEST_ALL2(T)                                                           \
  T (int8_t, uint16_t)                                                         \
  T (uint8_t, int16_t)                                                         \
  T (int16_t, uint32_t)                                                        \
  T (uint16_t, int32_t)                                                        \
  T (int32_t, double)                                                          \
  T (uint32_t, int64_t)                                                        \
  T (float, uint64_t)

TEST_ALL2 (test)

