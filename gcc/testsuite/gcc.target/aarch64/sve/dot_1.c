/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define DEF_DOT(TYPE1, TYPE2)						\
TYPE1 __attribute__ ((noinline, noclone))				\
dot_##TYPE1##_##TYPE2 (TYPE2 *restrict x, TYPE2 *restrict y, int n)	\
{									\
  TYPE1 sum = 0;							\
  for (int i = 0; i < n; i++)						\
    {									\
      sum += x[i] * y[i];						\
    }									\
  return sum;								\
}

DEF_DOT(uint32_t, uint8_t)
DEF_DOT(int32_t, int8_t)
DEF_DOT(int64_t, int16_t)

/* The uint16_t->uint64_t dot product requires a casting to satisfy the C
   language rules.  */
uint64_t __attribute__ ((noinline, noclone))
dot_uint64_t_uint16_t (uint16_t *restrict x, uint16_t *restrict y, int n)
{
  uint64_t sum = 0;
  for (int i = 0; i < n; i++)
    {
      sum += (unsigned int)x[i] * y[i];
    }
  return sum;
}

/* { dg-final { scan-assembler-times {\tudot\tz[0-9]+\.s, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsdot\tz[0-9]+\.s, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tudot\tz[0-9]+\.d, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsdot\tz[0-9]+\.d, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\twhilelo\t} 8 } } */
