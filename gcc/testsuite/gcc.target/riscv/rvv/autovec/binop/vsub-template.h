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

/* *int8_t not autovec currently. */
#define TEST_ALL()	\
 TEST_TYPE(int16_t)	\
 TEST_TYPE(uint16_t)	\
 TEST_TYPE(int32_t)	\
 TEST_TYPE(uint32_t)	\
 TEST_TYPE(int64_t)	\
 TEST_TYPE(uint64_t)    \
 TEST2_TYPE(int16_t)	\
 TEST2_TYPE(uint16_t)	\
 TEST2_TYPE(int32_t)	\
 TEST2_TYPE(uint32_t)	\
 TEST2_TYPE(int64_t)	\
 TEST2_TYPE(uint64_t)
 TEST3_TYPE(int16_t)	\
 TEST3_TYPE(uint16_t)	\
 TEST3_TYPE(int32_t)	\
 TEST3_TYPE(uint32_t)	\
 TEST3_TYPE(int64_t)	\
 TEST3_TYPE(uint64_t)	\
 TEST4_TYPE(int16_t)	\
 TEST4_TYPE(uint16_t)	\
 TEST4_TYPE(int32_t)	\
 TEST4_TYPE(uint32_t)	\
 TEST4_TYPE(int64_t)	\
 TEST4_TYPE(uint64_t)

TEST_ALL()
