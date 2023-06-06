#include <stdint-gcc.h>

#define TEST_TYPE(TYPE) 				\
  __attribute__((noipa))				\
  void vsub_##TYPE (TYPE *dst, TYPE *a, TYPE *b, int n)	\
  {							\
    for (int i = 0; i < n; i++)				\
      dst[i] = a[i] - b[i];				\
  }

#define TEST2_TYPE(TYPE) 				\
  __attribute__((noipa))				\
  void vsubs_##TYPE (TYPE *dst, TYPE *a, TYPE b, int n)	\
  {							\
    for (int i = 0; i < n; i++)				\
      dst[i] = a[i] - b;				\
  }

#define TEST3_TYPE(TYPE) 				\
  __attribute__((noipa))				\
  void vsubi_##TYPE (TYPE *dst, TYPE *a, int n)		\
  {							\
    for (int i = 0; i < n; i++)				\
      dst[i] = -16 - a[i];				\
  }

#define TEST4_TYPE(TYPE) 				\
  __attribute__((noipa))				\
  void vsubi2_##TYPE (TYPE *dst, TYPE *a, int n) 	\
  {							\
    for (int i = 0; i < n; i++)				\
      dst[i] = 15 - a[i];				\
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
 TEST2_TYPE(uint64_t)
 TEST2_TYPE(_Float16)	\
 TEST2_TYPE(float)	\
 TEST2_TYPE(double)	\

 TEST3_TYPE(int8_t)	\
 TEST3_TYPE(uint8_t)	\
 TEST3_TYPE(int16_t)	\
 TEST3_TYPE(uint16_t)	\
 TEST3_TYPE(int32_t)	\
 TEST3_TYPE(uint32_t)	\
 TEST3_TYPE(int64_t)	\
 TEST3_TYPE(uint64_t)	\
 TEST3_TYPE(_Float16)	\
 TEST3_TYPE(float)	\
 TEST3_TYPE(double)	\

 TEST4_TYPE(int8_t)	\
 TEST4_TYPE(uint8_t)	\
 TEST4_TYPE(int16_t)	\
 TEST4_TYPE(uint16_t)	\
 TEST4_TYPE(int32_t)	\
 TEST4_TYPE(uint32_t)	\
 TEST4_TYPE(int64_t)	\
 TEST4_TYPE(uint64_t)	\
 TEST4_TYPE(_Float16)	\
 TEST4_TYPE(float)	\
 TEST4_TYPE(double)	\

TEST_ALL()
