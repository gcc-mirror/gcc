#include <stdint-gcc.h>

#define TEST_TYPE(TYPE) 				\
  __attribute__((noipa))				\
  void vsqrt_##TYPE (TYPE *dst, TYPE *a, int n)		\
  {							\
    for (int i = 0; i < n; i++)				\
      dst[i] = __builtin_sqrtf (a[i]);			\
  }

#define TEST_TYPE2(TYPE) 				\
  __attribute__((noipa))				\
  void vsqrt_##TYPE (TYPE *dst, TYPE *a, int n)		\
  {							\
    for (int i = 0; i < n; i++)				\
      dst[i] = __builtin_sqrt (a[i]);			\
  }

#define TEST_TYPE3(TYPE) 				\
  __attribute__((noipa))				\
  void vsqrt_##TYPE (TYPE *dst, TYPE *a, int n)		\
  {							\
    for (int i = 0; i < n; i++)				\
      dst[i] = __builtin_sqrtf16 (a[i]);		\
  }

#define TEST_ALL()					\
 TEST_TYPE(float)					\
 TEST_TYPE2(double)					\

TEST_ALL()
