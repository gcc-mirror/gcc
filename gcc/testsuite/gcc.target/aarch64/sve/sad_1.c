/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define DEF_SAD(TYPE1, TYPE2)						\
TYPE1 __attribute__ ((noinline, noclone))				\
sum_abs_##TYPE1##_##TYPE2 (TYPE2 *restrict x, TYPE2 *restrict y, int n)	\
{									\
  TYPE1 sum = 0;							\
  for (int i = 0; i < n; i++)						\
    {									\
      sum += __builtin_abs (x[i] - y[i]);				\
    }									\
  return sum;								\
}

DEF_SAD(int32_t, uint8_t)
DEF_SAD(int32_t, int8_t)
DEF_SAD(int64_t, uint16_t)
DEF_SAD(int64_t, int16_t)

/* { dg-final { scan-assembler-times {\tuabd\tz[0-9]+\.b, p[0-7]/m, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsabd\tz[0-9]+\.b, p[0-7]/m, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tudot\tz[0-9]+\.s, z[0-9]+\.b, z[0-9]+\.b\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuabd\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsabd\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tudot\tz[0-9]+\.d, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
