#include <stdint.h>

#pragma GCC target "+nosve"

#define N 100

#define DEF_FUNC(TYPE, B1, B2, C1, C2)					\
  void __attribute__ ((noipa))						\
  f_##TYPE (TYPE *restrict a, TYPE *restrict b, TYPE *restrict c)	\
  {									\
    for (int i = 0; i < N; ++i)						\
      a[i] = ((__int128) b[i] + c[i] + BIAS) >> 1;			\
  }

#define TEST_FUNC(TYPE, B1, B2, C1, C2)					\
  {									\
    TYPE a[N], b[N], c[N];						\
    for (TYPE i = 0; i < N; ++i)					\
      {									\
	b[i] = B1 + i * B2;						\
	c[i] = C1 + i * C2;						\
      }									\
    f_##TYPE (a, b, c);							\
    for (TYPE i = 0; i < N; ++i)					\
      if (a[i] != ((B1 + C1 + BIAS + (__int128) i * (B2 + C2)) >> 1))	\
	__builtin_abort ();						\
  }

#define FOR_EACH_SIGNED_TYPE(T) \
  T (int8_t, -124, 2, -40, 1) \
  T (int16_t, -32000, 510, -10000, 257) \
  T (int32_t, -2000000000, 131072, -3277000, 65537) \
  T (int64_t, -44, 100, -10000, 99)

#define FOR_EACH_UNSIGNED_TYPE(T) \
  T (uint8_t, 4, 2, 40, 1) \
  T (uint16_t, 12, 510, 10000, 257) \
  T (uint32_t, 20, 131072, 3277000, 65537) \
  T (uint64_t, 90, 100, 10000, 99)
