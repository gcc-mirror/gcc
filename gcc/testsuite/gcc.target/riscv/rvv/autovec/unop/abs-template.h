#include <stdint-gcc.h>

#define TEST_TYPE(TYPE) 				\
  __attribute__((noipa))				\
  void vabs_##TYPE (TYPE *dst, TYPE *a, int n)		\
  {							\
    for (int i = 0; i < n; i++)				\
      dst[i] = __builtin_abs (a[i]);				\
  }

#define TEST_TYPE2(TYPE) 				\
  __attribute__((noipa))				\
  void vabs_##TYPE (TYPE *dst, TYPE *a, int n)		\
  {							\
    for (int i = 0; i < n; i++)				\
      dst[i] = __builtin_llabs (a[i]);				\
  }

#define TEST_TYPE3(TYPE) 				\
  __attribute__((noipa))				\
  void vabs_##TYPE (TYPE *dst, TYPE *a, int n)		\
  {							\
    for (int i = 0; i < n; i++)				\
      dst[i] = __builtin_fabs (a[i]);				\
  }

#define TEST_ALL()	\
 TEST_TYPE(int8_t)	\
 TEST_TYPE(int16_t)	\
 TEST_TYPE(int32_t)	\
 TEST_TYPE2(int64_t)	\
 TEST_TYPE3(_Float16)	\
 TEST_TYPE3(float)	\
 TEST_TYPE3(double)	\

TEST_ALL()
