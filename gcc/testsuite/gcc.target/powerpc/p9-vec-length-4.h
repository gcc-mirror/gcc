#include "p9-vec-length.h"

/* Test the case that the loop which has multiple vectors (concatenated vectors)
   but with same vector type.  */

#define test(TYPE)                                                             \
  void __attribute__ ((noinline, noclone))                                     \
    test_mv_##TYPE (TYPE *restrict a, TYPE *restrict b, TYPE *restrict c,      \
		    int n)                                                     \
  {                                                                            \
    for (int i = 0; i < n; ++i)                                                \
      {                                                                        \
	a[i] += 1;                                                             \
	b[i * 2] += 2;                                                         \
	b[i * 2 + 1] += 3;                                                     \
	c[i * 4] += 4;                                                         \
	c[i * 4 + 1] += 5;                                                     \
	c[i * 4 + 2] += 6;                                                     \
	c[i * 4 + 3] += 7;                                                     \
      }                                                                        \
  }

TEST_ALL (test)

