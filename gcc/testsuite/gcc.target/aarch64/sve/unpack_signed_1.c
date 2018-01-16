/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fno-inline" } */

#include <stdint.h>

#define UNPACK(TYPED, TYPES)						\
void __attribute__ ((noinline, noclone))				\
unpack_##TYPED##_##TYPES (TYPED *d, TYPES *s, TYPES mask, int size)	\
{									\
  for (int i = 0; i < size; i++)					\
    d[i] = (TYPES) (s[i] | mask);					\
}

#define TEST_ALL(T)			\
  T (int64_t, int32_t)			\
  T (int32_t, int16_t)			\
  T (int16_t, int8_t)			\
  T (uint64_t, int32_t)			\
  T (uint32_t, int16_t)			\
  T (uint16_t, int8_t)

TEST_ALL (UNPACK)

/* { dg-final { scan-assembler-times {\tsunpkhi\tz[0-9]+\.d, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tsunpkhi\tz[0-9]+\.s, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tsunpkhi\tz[0-9]+\.h, z[0-9]+\.b\n} 2 } } */

/* { dg-final { scan-assembler-times {\tsunpklo\tz[0-9]+\.d, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tsunpklo\tz[0-9]+\.s, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tsunpklo\tz[0-9]+\.h, z[0-9]+\.b\n} 2 } } */
