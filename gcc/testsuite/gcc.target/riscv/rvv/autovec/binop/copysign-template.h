#include <stdint-gcc.h>

#define TEST_TYPE(TYPE, SUFFIX)					\
  __attribute__((noipa))					\
  void copysign_##TYPE (TYPE *restrict dst, TYPE *restrict a,	\
			TYPE *restrict b, int n)		\
  {								\
    for (int i = 0; i < n; i++)					\
      dst[i] = __builtin_copysign##SUFFIX (a[i], b[i]);		\
  }

#define TEST_TYPE2(TYPE, SUFFIX)				\
  __attribute__((noipa))					\
  void copysigns_##TYPE (TYPE *restrict dst, TYPE *restrict a,	\
			 TYPE b, int n)				\
  {								\
    for (int i = 0; i < n; i++)					\
      dst[i] = __builtin_copysign##SUFFIX (a[i], b);		\
  }

#define TEST_TYPE3(TYPE, SUFFIX)				\
  __attribute__((noipa))					\
  void xorsign_##TYPE (TYPE *restrict dst, TYPE *restrict a,	\
		       TYPE *restrict b, int n)			\
  {								\
    for (int i = 0; i < n; i++)					\
      dst[i] = a[i] * __builtin_copysign##SUFFIX (1.0, b[i]);	\
  }

#define TEST_TYPE4(TYPE, SUFFIX)				\
  __attribute__((noipa))					\
  void xorsigns_##TYPE (TYPE *restrict dst, TYPE *restrict a,	\
			TYPE b, int n)				\
  {								\
    for (int i = 0; i < n; i++)					\
      dst[i] = a[i] * __builtin_copysign##SUFFIX (1.0, b);	\
  }

#define TEST_TYPE5(TYPE, SUFFIX)				\
  __attribute__((noipa))					\
  void ncopysign_##TYPE (TYPE *restrict dst, TYPE *restrict a,  \
			 TYPE *restrict b, int n)		\
  {								\
    for (int i = 0; i < n; i++)					\
      dst[i] = -__builtin_copysign##SUFFIX (a[i], b[i]);	\
  }

#define TEST_TYPE6(TYPE, SUFFIX)				\
  __attribute__((noipa))					\
  void ncopysigns_##TYPE (TYPE *restrict dst, TYPE *restrict a, \
			  TYPE b, int n)			\
  {								\
    for (int i = 0; i < n; i++)					\
      dst[i] = -__builtin_copysign##SUFFIX (a[i], b);		\
  }


#define TEST_ALL()						\
 TEST_TYPE(_Float16,f16)					\
 TEST_TYPE(float,f)						\
 TEST_TYPE(double,)						\
 TEST_TYPE2(_Float16,f16)					\
 TEST_TYPE2(float,f)						\
 TEST_TYPE2(double,)						\
 TEST_TYPE3(_Float16,f16)					\
 TEST_TYPE3(float,f)						\
 TEST_TYPE3(double,)						\
 TEST_TYPE4(_Float16,f16)					\
 TEST_TYPE4(float,f)						\
 TEST_TYPE4(double,)						\
 TEST_TYPE5(_Float16,f16)					\
 TEST_TYPE5(float,f)						\
 TEST_TYPE5(double,)						\
 TEST_TYPE6(_Float16,f16)					\
 TEST_TYPE6(float,f)						\
 TEST_TYPE6(double,)						\

TEST_ALL()
