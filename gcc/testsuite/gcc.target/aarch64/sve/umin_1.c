/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O3 --save-temps" } */

#include <stdint.h>

#define DO_REGREG_OPS(TYPE)					\
void varith_##TYPE##_reg (TYPE *dst, TYPE *src, int count)	\
{								\
  for (int i = 0; i < count; ++i)				\
    dst[i] = dst[i] < src[i] ? dst[i] : src[i];			\
}

#define DO_IMMEDIATE_OPS(VALUE, TYPE)				\
void varithimm_##VALUE##_##TYPE (TYPE *dst, int count)		\
{								\
  for (int i = 0; i < count; ++i)				\
    dst[i] = dst[i] < (TYPE) VALUE ? dst[i] : (TYPE) VALUE;	\
}

#define DO_ARITH_OPS(TYPE)		\
  DO_REGREG_OPS (TYPE);			\
  DO_IMMEDIATE_OPS (2, TYPE);		\
  DO_IMMEDIATE_OPS (86, TYPE);		\
  DO_IMMEDIATE_OPS (109, TYPE);		\
  DO_IMMEDIATE_OPS (141, TYPE);		\
  DO_IMMEDIATE_OPS (229, TYPE);		\
  DO_IMMEDIATE_OPS (255, TYPE);		\
  DO_IMMEDIATE_OPS (256, TYPE);

DO_ARITH_OPS (uint8_t)
DO_ARITH_OPS (uint16_t)
DO_ARITH_OPS (uint32_t)
DO_ARITH_OPS (uint64_t)

/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.b, p[0-7]/m, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.b, z[0-9]+\.b, #86\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.b, z[0-9]+\.b, #109\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.b, z[0-9]+\.b, #141\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.b, z[0-9]+\.b, #229\n} 1 } } */
/* { dg-final { scan-assembler-not {\tumin\tz[0-9]+\.b, z[0-9]+\.b, #255\n} } } */
/* { dg-final { scan-assembler-not {\tumin\tz[0-9]+\.b, z[0-9]+\.b, #256\n} } } */

/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.h, z[0-9]+\.h, #86\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.h, z[0-9]+\.h, #109\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.h, z[0-9]+\.h, #141\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.h, z[0-9]+\.h, #229\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.h, z[0-9]+\.h, #255\n} 1 } } */
/* { dg-final { scan-assembler-not {\tumin\tz[0-9]+\.h, z[0-9]+\.h, #256\n} } } */

/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.s, z[0-9]+\.s, #86\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.s, z[0-9]+\.s, #109\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.s, z[0-9]+\.s, #141\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.s, z[0-9]+\.s, #229\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.s, z[0-9]+\.s, #255\n} 1 } } */
/* { dg-final { scan-assembler-not {\tumin\tz[0-9]+\.s, z[0-9]+\.s, #256\n} } } */

/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 2 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.d, z[0-9]+\.d, #86\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.d, z[0-9]+\.d, #109\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.d, z[0-9]+\.d, #141\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.d, z[0-9]+\.d, #229\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.d, z[0-9]+\.d, #255\n} 1 } } */
/* { dg-final { scan-assembler-not {\tumin\tz[0-9]+\.d, z[0-9]+\.d, #256\n} } } */
