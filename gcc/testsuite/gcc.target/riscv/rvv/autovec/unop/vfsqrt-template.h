#include <stdint-gcc.h>

#define TEST_TYPE(TYPE, SUFFIX) 				\
  __attribute__((noipa))				\
  void vsqrt_##TYPE (TYPE *dst, TYPE *a, int n)		\
  {							\
    for (int i = 0; i < n; i++)				\
      dst[i] = __builtin_sqrt##SUFFIX (a[i]);			\
  }

#define TEST_ALL()					\
 TEST_TYPE(float, f)					\
 TEST_TYPE(double, )					\

TEST_ALL()
