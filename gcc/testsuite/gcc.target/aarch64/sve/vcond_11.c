/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -march=armv8-a+sve --param aarch64-vect-compare-costs=0" } */

#include <stdint.h>

#define DEF_LOOP(TYPE)							\
  void __attribute__ ((noinline, noclone))				\
  test_##TYPE (int *restrict a, TYPE *restrict b, int a1, int a2,	\
	       int a3, int a4, int n)					\
  {									\
    for (int i = 0; i < n; i += 2)					\
      {									\
	a[i] = a[i] >= 1 & b[i] != 3 ? a1 : a2;				\
	a[i + 1] = a[i + 1] >= 1 & b[i + 1] != 3 ? a3 : a4;		\
      }									\
  }

#define FOR_EACH_TYPE(T) \
  T (int8_t) \
  T (uint8_t) \
  T (int16_t) \
  T (uint16_t) \
  T (int64_t) \
  T (uint64_t) \
  T (double)

FOR_EACH_TYPE (DEF_LOOP)

/* { dg-final { scan-assembler-times {\tld1b\t} 2 } } */
/* { dg-final { scan-assembler-times {\tld1h\t} 2 } } */
/* 4 for each 8-bit function, 2 for each 16-bit function, 1 for
   each 64-bit function.  */
/* { dg-final { scan-assembler-times {\tld1w\t} 15 } } */
/* 3 64-bit functions * 2 64-bit vectors per 32-bit vector.  */
/* { dg-final { scan-assembler-times {\tld1d\t} 6 } } */
/* { dg-final { scan-assembler-times {\tsel\tz[0-9]} 15 } } */
