#include "p9-vec-length.h"

/* Test the case that the loop which has the same concatenated vectors (same
   size per iteration) but from different types.  */

#define test(TYPE1, TYPE2)                                                     \
  void __attribute__ ((noinline, noclone))                                     \
    test_mv_##TYPE1##TYPE2 (TYPE1 *restrict a, TYPE2 *restrict b, int n)       \
  {                                                                            \
    for (int i = 0; i < n; i++)                                                \
      {                                                                        \
	a[i * 2] += 1;                                                         \
	a[i * 2 + 1] += 2;                                                     \
	b[i * 4] += 3;                                                         \
	b[i * 4 + 1] += 4;                                                     \
	b[i * 4 + 2] += 5;                                                     \
	b[i * 4 + 3] += 6;                                                     \
      }                                                                        \
  }

#define TEST_ALL2(T)                                                           \
  T (int16_t, uint8_t)                                                         \
  T (uint16_t, int8_t)                                                         \
  T (int32_t, uint16_t)                                                        \
  T (uint32_t, int16_t)                                                        \
  T (float, uint16_t)                                                          \
  T (int64_t, float)                                                           \
  T (uint64_t, int32_t)                                                        \
  T (double, uint32_t)

TEST_ALL2 (test)

