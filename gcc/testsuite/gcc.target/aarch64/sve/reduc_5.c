/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math" } */

#include <stdint.h>

#define REDUC(TYPE)						\
  TYPE reduc_##TYPE (TYPE *x, int count)			\
  {								\
    TYPE sum = 0;						\
    for (int i = 0; i < count; ++i)				\
      sum -= x[i];						\
    return sum;							\
  }

REDUC (int8_t)
REDUC (uint8_t)
REDUC (int16_t)
REDUC (uint16_t)
REDUC (int32_t)
REDUC (uint32_t)
REDUC (int64_t)
REDUC (uint64_t)
REDUC (float)
REDUC (double)

/* XFAILed until we support sub-int reductions for signed types.  */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.b, p[0-7]/m} 2 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.h, p[0-7]/m} 2 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.b, p[0-7]/m} 1 } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.h, p[0-7]/m} 1 } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.s, p[0-7]/m} 2 } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.d, p[0-7]/m} 2 } } */
/* { dg-final { scan-assembler-times {\tfsub\tz[0-9]+\.s, p[0-7]/m} 1 } } */
/* { dg-final { scan-assembler-times {\tfsub\tz[0-9]+\.d, p[0-7]/m} 1 } } */

/* XFAILed until we support sub-int reductions for signed types.  */
/* { dg-final { scan-assembler-times {\tsub\t} 8 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tfsub\t} 2 } } */
