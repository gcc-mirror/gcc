/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-details --save-temps" } */

#include <stdint.h>

#define MULTHI(TYPE, BIGGER, RND)                     \
TYPE __attribute__ ((noinline, noclone))              \
mulhs_##TYPE##_##RND (TYPE *restrict x,               \
        TYPE *restrict y, TYPE *restrict z, int n)    \
{                                                     \
  for (int i = 0; i < n; i++)                         \
  {                                                   \
    z[i] = ((((BIGGER)x[i] * (BIGGER)y[i]) >>         \
            (sizeof(BIGGER)*8/2-2)) + RND) >> 1;      \
  }                                                   \
}

MULTHI (int8_t, int16_t, 0)
MULTHI (int16_t, int32_t, 0)
MULTHI (int32_t, int64_t, 0)

MULTHI (uint8_t, uint16_t, 0)
MULTHI (uint16_t, uint32_t, 0)
MULTHI (uint32_t, uint64_t, 0)

MULTHI (int8_t, int16_t, 1)
MULTHI (int16_t, int32_t, 1)
MULTHI (int32_t, int64_t, 1)

MULTHI (uint8_t, uint16_t, 1)
MULTHI (uint16_t, uint32_t, 1)
MULTHI (uint32_t, uint64_t, 1)

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 12 "vect" } } */

/* { dg-final { scan-assembler-times {\tsmullb\tz[0-9]+\.h, z[0-9]+\.b, z[0-9]+\.b\n} 2 } } */
/* { dg-final { scan-assembler-times {\tsmullt\tz[0-9]+\.h, z[0-9]+\.b, z[0-9]+\.b\n} 2 } } */
/* { dg-final { scan-assembler-times {\tsmullb\tz[0-9]+\.s, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tsmullt\tz[0-9]+\.s, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tsmullb\tz[0-9]+\.d, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tsmullt\tz[0-9]+\.d, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */

/* { dg-final { scan-assembler-times {\tshrnb\tz[0-9]+\.b, z[0-9]+\.h, #7\n} 2 } } */
/* { dg-final { scan-assembler-times {\tshrnt\tz[0-9]+\.b, z[0-9]+\.h, #7\n} 2 } } */
/* { dg-final { scan-assembler-times {\tshrnb\tz[0-9]+\.h, z[0-9]+\.s, #15\n} 2 } } */
/* { dg-final { scan-assembler-times {\tshrnt\tz[0-9]+\.h, z[0-9]+\.s, #15\n} 2 } } */
/* { dg-final { scan-assembler-times {\tshrnb\tz[0-9]+\.s, z[0-9]+\.d, #31\n} 2 } } */
/* { dg-final { scan-assembler-times {\tshrnt\tz[0-9]+\.s, z[0-9]+\.d, #31\n} 2 } } */

/* { dg-final { scan-assembler-times {\tumullb\tz[0-9]+\.h, z[0-9]+\.b, z[0-9]+\.b\n} 2 } } */
/* { dg-final { scan-assembler-times {\tumullt\tz[0-9]+\.h, z[0-9]+\.b, z[0-9]+\.b\n} 2 } } */
/* { dg-final { scan-assembler-times {\tumullb\tz[0-9]+\.s, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tumullt\tz[0-9]+\.s, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tumullb\tz[0-9]+\.d, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tumullt\tz[0-9]+\.d, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */

/* { dg-final { scan-assembler-times {\trshrnb\tz[0-9]+\.b, z[0-9]+\.h, #7\n} 2 } } */
/* { dg-final { scan-assembler-times {\trshrnt\tz[0-9]+\.b, z[0-9]+\.h, #7\n} 2 } } */
/* { dg-final { scan-assembler-times {\trshrnb\tz[0-9]+\.h, z[0-9]+\.s, #15\n} 2 } } */
/* { dg-final { scan-assembler-times {\trshrnt\tz[0-9]+\.h, z[0-9]+\.s, #15\n} 2 } } */
/* { dg-final { scan-assembler-times {\trshrnb\tz[0-9]+\.s, z[0-9]+\.d, #31\n} 2 } } */
/* { dg-final { scan-assembler-times {\trshrnt\tz[0-9]+\.s, z[0-9]+\.d, #31\n} 2 } } */

