/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-details --save-temps" } */

#include <stdint.h>

#define AVERAGE(TYPE, BIGGER, RND)					  \
TYPE __attribute__ ((noinline, noclone))				  \
avg_##TYPE##_##RND (TYPE *restrict x, TYPE *restrict y, TYPE *restrict z, \
		    int n)						  \
{									  \
  for (int i = 0; i < n; i++)						  \
    {									  \
      z[i] = ((BIGGER)x[i] + y[i] + RND) >> 1;				  \
    }									  \
}

AVERAGE (int8_t, int64_t, 0)
AVERAGE (int16_t, int64_t, 0)
AVERAGE (int32_t, int64_t, 0)
AVERAGE (uint8_t, uint64_t, 0)
AVERAGE (uint16_t, uint64_t, 0)
AVERAGE (uint32_t, uint64_t, 0)
AVERAGE (int8_t, int64_t, 1)
AVERAGE (int16_t, int64_t, 1)
AVERAGE (int32_t, int64_t, 1)
AVERAGE (uint8_t, uint64_t, 1)
AVERAGE (uint16_t, uint64_t, 1)
AVERAGE (uint32_t, uint64_t, 1)

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 12 "vect" } } */

/* { dg-final { scan-assembler-times {\tshadd\tz[0-9]+\.b, p[0-7]/m, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tshadd\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tshadd\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */

/* { dg-final { scan-assembler-times {\tuhadd\tz[0-9]+\.b, p[0-7]/m, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuhadd\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuhadd\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */

/* { dg-final { scan-assembler-times {\tsrhadd\tz[0-9]+\.b, p[0-7]/m, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsrhadd\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsrhadd\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */

/* { dg-final { scan-assembler-times {\turhadd\tz[0-9]+\.b, p[0-7]/m, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\turhadd\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\turhadd\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
