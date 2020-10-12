#include "p9-vec-length.h"

/* Test the case that the loop requires to peel for gaps.  */

#define N 200

#define test(TYPE)                                                             \
  void __attribute__((noinline, noclone))                                      \
      test_##TYPE(TYPE *restrict dest, TYPE *restrict src) {                   \
    for (unsigned int i = 0; i < N; ++i)                                       \
      dest[i] += src[i * 2];                                                   \
  }

TEST_ALL(test)
