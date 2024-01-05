#include <stdint-gcc.h>

#define TEST_TYPE(TYPE) 				\
  __attribute__((noipa))				\
  void vneg_##TYPE (TYPE *dst, TYPE *a, int n)		\
  {							\
    for (int i = 0; i < n; i++)				\
      dst[i] =  -a[i];					\
  }

#define TEST_ALL()					\
 TEST_TYPE(int8_t)					\
 TEST_TYPE(int16_t)					\
 TEST_TYPE(int32_t)					\
 TEST_TYPE(int64_t)					\
 TEST_TYPE(_Float16)					\
 TEST_TYPE(float)					\
 TEST_TYPE(double)					\

TEST_ALL()
