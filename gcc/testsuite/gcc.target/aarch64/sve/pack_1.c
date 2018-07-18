/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define PACK(TYPED, TYPES)				\
void __attribute__ ((noinline, noclone))		\
pack_##TYPED##_##TYPES (TYPED *d, TYPES *s, int size)	\
{							\
  for (int i = 0; i < size; i++)			\
    d[i] = s[i] + 1;					\
}

#define TEST_ALL(T)				\
  T (int32_t, int64_t)				\
  T (int16_t, int32_t)				\
  T (int8_t, int16_t)				\
  T (uint32_t, uint64_t)			\
  T (uint16_t, uint32_t)			\
  T (uint8_t, uint16_t)

TEST_ALL (PACK)

/* { dg-final { scan-assembler-times {\tuzp1\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuzp1\tz[0-9]+\.h, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuzp1\tz[0-9]+\.b, z[0-9]+\.b, z[0-9]+\.b\n} 2 } } */
