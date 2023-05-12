#include <stdlib.h>
#include <stdint-gcc.h>

#define TEST_TYPE(TYPE) 				\
  __attribute__((noipa))				\
  void vabs_##TYPE (TYPE *dst, TYPE *a, int n)		\
  {							\
    for (int i = 0; i < n; i++)				\
      dst[i] = abs (a[i]);				\
  }

#define TEST_TYPE2(TYPE) 				\
  __attribute__((noipa))				\
  void vabs_##TYPE (TYPE *dst, TYPE *a, int n)		\
  {							\
    for (int i = 0; i < n; i++)				\
      dst[i] = llabs (a[i]);				\
  }

#define TEST_ALL()	\
 TEST_TYPE(int8_t)	\
 TEST_TYPE(int16_t)	\
 TEST_TYPE(int32_t)	\
 TEST_TYPE2(int64_t)

TEST_ALL()
