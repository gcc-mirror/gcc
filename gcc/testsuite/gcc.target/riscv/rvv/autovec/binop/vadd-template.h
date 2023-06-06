#include <stdint-gcc.h>

#define TEST_TYPE(TYPE) 				\
  __attribute__((noipa))				\
  void vadd_##TYPE (TYPE *dst, TYPE *a, TYPE *b, int n)	\
  {							\
    for (int i = 0; i < n; i++)				\
      dst[i] = a[i] + b[i];				\
  }

#define TEST2_TYPE(TYPE) 				\
  __attribute__((noipa))				\
  void vadds_##TYPE (TYPE *dst, TYPE *a, TYPE b, int n)	\
  {							\
    for (int i = 0; i < n; i++)				\
      dst[i] = a[i] + b;				\
  }

#define TEST3_TYPE(TYPE) 				\
  __attribute__((noipa))				\
  void vaddi_##TYPE (TYPE *dst, TYPE *a, int n)	        \
  {							\
    for (int i = 0; i < n; i++)				\
      dst[i] = a[i] + 15;				\
  }

#define TEST3M_TYPE(TYPE) 				\
  __attribute__((noipa))				\
  void vaddim_##TYPE (TYPE *dst, TYPE *a, int n)	\
  {							\
    for (int i = 0; i < n; i++)				\
      dst[i] = a[i] - 16;				\
  }

#define TEST_ALL()	\
 TEST_TYPE(int8_t)	\
 TEST_TYPE(uint8_t)	\
 TEST_TYPE(int16_t)	\
 TEST_TYPE(uint16_t)	\
 TEST_TYPE(int32_t)	\
 TEST_TYPE(uint32_t)	\
 TEST_TYPE(int64_t)	\
 TEST_TYPE(uint64_t)    \
 TEST_TYPE(_Float16)	\
 TEST_TYPE(float)	\
 TEST_TYPE(double)	\
 TEST2_TYPE(int8_t)	\
 TEST2_TYPE(uint8_t)	\
 TEST2_TYPE(int16_t)	\
 TEST2_TYPE(uint16_t)	\
 TEST2_TYPE(int32_t)	\
 TEST2_TYPE(uint32_t)	\
 TEST2_TYPE(int64_t)	\
 TEST2_TYPE(uint64_t)   \
 TEST2_TYPE(_Float16)	\
 TEST2_TYPE(float)	\
 TEST2_TYPE(double)	\
 TEST3M_TYPE(int8_t)	\
 TEST3_TYPE(uint8_t)	\
 TEST3M_TYPE(int16_t)	\
 TEST3_TYPE(uint16_t)	\
 TEST3M_TYPE(int32_t)	\
 TEST3_TYPE(uint32_t)	\
 TEST3M_TYPE(int64_t)	\
 TEST3_TYPE(uint64_t)   \
 TEST3M_TYPE(_Float16)	\
 TEST3_TYPE(float)	\
 TEST3M_TYPE(double)	\

TEST_ALL()
