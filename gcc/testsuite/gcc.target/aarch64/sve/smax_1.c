/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O3 --save-temps" } */

#include <stdint.h>

#define DO_REGREG_OPS(TYPE)					\
void varith_##TYPE##_reg (TYPE *dst, TYPE *src, int count)	\
{								\
  for (int i = 0; i < count; ++i)				\
    dst[i] = dst[i] > src[i] ? dst[i] : src[i];			\
}

#define DO_IMMEDIATE_OPS(VALUE, TYPE, NAME)			\
void varithimm_##NAME##_##TYPE (TYPE *dst, int count)		\
{								\
  for (int i = 0; i < count; ++i)				\
    dst[i] = dst[i] > (TYPE) VALUE ? dst[i] : (TYPE) VALUE;	\
}

#define DO_ARITH_OPS(TYPE)			\
  DO_REGREG_OPS (TYPE);				\
  DO_IMMEDIATE_OPS (0, TYPE, 0);		\
  DO_IMMEDIATE_OPS (86, TYPE, 86);		\
  DO_IMMEDIATE_OPS (109, TYPE, 109);		\
  DO_IMMEDIATE_OPS (141, TYPE, 141);		\
  DO_IMMEDIATE_OPS (-1, TYPE, minus1);		\
  DO_IMMEDIATE_OPS (-110, TYPE, minus110);	\
  DO_IMMEDIATE_OPS (-141, TYPE, minus141);

DO_ARITH_OPS (int8_t)
DO_ARITH_OPS (int16_t)
DO_ARITH_OPS (int32_t)
DO_ARITH_OPS (int64_t)

/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.b, p[0-7]/m, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.b, z[0-9]+\.b, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.b, z[0-9]+\.b, #86\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.b, z[0-9]+\.b, #109\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.b, z[0-9]+\.b, #115\n} 1 } } */
/* { dg-final { scan-assembler-not {\tsmax\tz[0-9]+\.b, z[0-9]+\.b, #141\n} } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.b, z[0-9]+\.b, #-1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.b, z[0-9]+\.b, #-110\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.b, z[0-9]+\.b, #-115\n} 1 } } */
/* { dg-final { scan-assembler-not {\tsmax\tz[0-9]+\.b, z[0-9]+\.b, #-141\n} } } */

/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 3 } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.h, z[0-9]+\.h, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.h, z[0-9]+\.h, #86\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.h, z[0-9]+\.h, #109\n} 1 } } */
/* { dg-final { scan-assembler-not {\tsmax\tz[0-9]+\.h, z[0-9]+\.h, #141\n} } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.h, z[0-9]+\.h, #-1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.h, z[0-9]+\.h, #-110\n} 1 } } */
/* { dg-final { scan-assembler-not {\tsmax\tz[0-9]+\.h, z[0-9]+\.h, #-141\n} } } */

/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 3 } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.s, z[0-9]+\.s, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.s, z[0-9]+\.s, #86\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.s, z[0-9]+\.s, #109\n} 1 } } */
/* { dg-final { scan-assembler-not {\tsmax\tz[0-9]+\.s, z[0-9]+\.s, #141\n} } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.s, z[0-9]+\.s, #-1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.s, z[0-9]+\.s, #-110\n} 1 } } */
/* { dg-final { scan-assembler-not {\tsmax\tz[0-9]+\.s, z[0-9]+\.s, #-141\n} } } */

/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 3 } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.d, z[0-9]+\.d, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.d, z[0-9]+\.d, #86\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.d, z[0-9]+\.d, #109\n} 1 } } */
/* { dg-final { scan-assembler-not {\tsmax\tz[0-9]+\.d, z[0-9]+\.d, #141\n} } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.d, z[0-9]+\.d, #-1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsmax\tz[0-9]+\.d, z[0-9]+\.d, #-110\n} 1 } } */
/* { dg-final { scan-assembler-not {\tsmax\tz[0-9]+\.d, z[0-9]+\.d, #-141\n} } } */
