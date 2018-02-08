/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O3 --save-temps" } */

#include <stdint.h>

#define DO_REGREG_OPS(TYPE, OP, NAME)				\
void varith_##TYPE##_##NAME (TYPE *dst, TYPE *src, int count)	\
{								\
  for (int i = 0; i < count; ++i)				\
    dst[i] = dst[i] OP src[i];					\
}

#define DO_IMMEDIATE_OPS(VALUE, TYPE, OP, NAME)		\
void varithimm_##NAME##_##TYPE (TYPE *dst, int count)	\
{							\
  for (int i = 0; i < count; ++i)			\
    dst[i] = dst[i] OP VALUE;				\
}

#define DO_ARITH_OPS(TYPE, OP, NAME)			\
  DO_REGREG_OPS (TYPE, OP, NAME);			\
  DO_IMMEDIATE_OPS (0, TYPE, OP, NAME ## 0);		\
  DO_IMMEDIATE_OPS (5, TYPE, OP, NAME ## 5);		\
  DO_IMMEDIATE_OPS (255, TYPE, OP, NAME ## 255);	\
  DO_IMMEDIATE_OPS (256, TYPE, OP, NAME ## 256);	\
  DO_IMMEDIATE_OPS (257, TYPE, OP, NAME ## 257);	\
  DO_IMMEDIATE_OPS (65280, TYPE, OP, NAME ## 65280);	\
  DO_IMMEDIATE_OPS (65281, TYPE, OP, NAME ## 65281);	\
  DO_IMMEDIATE_OPS (-1, TYPE, OP, NAME ## minus1);

DO_ARITH_OPS (int8_t, +, add)
DO_ARITH_OPS (int16_t, +, add)
DO_ARITH_OPS (int32_t, +, add)
DO_ARITH_OPS (int64_t, +, add)
DO_ARITH_OPS (int8_t, -, minus)
DO_ARITH_OPS (int16_t, -, minus)
DO_ARITH_OPS (int32_t, -, minus)
DO_ARITH_OPS (int64_t, -, minus)

/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.b, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.b, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.h, z[0-9]+\.h, z[0-9]+\.h\n} 3 } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.h, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s\n} 5 } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 5 } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */

/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.b, z[0-9]+\.b, #1\n} 4 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.b, z[0-9]+\.b, #5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.b, z[0-9]+\.b, #251\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.b, z[0-9]+\.b, #255\n} 4 } } */
/* { dg-final { scan-assembler-not {\tadd\tz[0-9]+\.b, z[0-9]+\.b, #256\n} } } */
/* { dg-final { scan-assembler-not {\tadd\tz[0-9]+\.b, z[0-9]+\.b, #257\n} } } */
/* { dg-final { scan-assembler-not {\tadd\tz[0-9]+\.b, z[0-9]+\.b, #65280\n} } } */
/* { dg-final { scan-assembler-not {\tadd\tz[0-9]+\.b, z[0-9]+\.b, #65281\n} } } */
/* { dg-final { scan-assembler-not {\tadd\tz[0-9]+\.b, z[0-9]+\.b, #-1\n} } } */
/* { dg-final { scan-assembler-not {\tsub\tz[0-9]+\.b, z[0-9]+\.b, #1\n} } } */

/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.h, z[0-9]+\.h, #1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.h, z[0-9]+\.h, #5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.h, z[0-9]+\.h, #255\n} 2 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.h, z[0-9]+\.h, #256\n} 2 } } */
/* { dg-final { scan-assembler-not {\tadd\tz[0-9]+\.h, z[0-9]+\.h, #257\n} } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.h, z[0-9]+\.h, #65280\n} 2 } } */
/* { dg-final { scan-assembler-not {\tadd\tz[0-9]+\.h, z[0-9]+\.h, #65281\n} } } */
/* { dg-final { scan-assembler-not {\tadd\tz[0-9]+\.h, z[0-9]+\.h, #-1\n} } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.h, z[0-9]+\.h, #1\n} 1 } } */

/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.s, z[0-9]+\.s, #5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.s, z[0-9]+\.s, #255\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.s, z[0-9]+\.s, #256\n} 1 } } */
/* { dg-final { scan-assembler-not {\tadd\tz[0-9]+\.s, z[0-9]+\.s, #257\n} } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.s, z[0-9]+\.s, #65280\n} 1 } } */
/* { dg-final { scan-assembler-not {\tadd\tz[0-9]+\.s, z[0-9]+\.s, #65281\n} } } */
/* { dg-final { scan-assembler-not {\tadd\tz[0-9]+\.s, z[0-9]+\.s, #-1\n} } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.s, z[0-9]+\.s, #1\n} 1 } } */

/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.d, z[0-9]+\.d, #5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.d, z[0-9]+\.d, #255\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.d, z[0-9]+\.d, #256\n} 1 } } */
/* { dg-final { scan-assembler-not {\tadd\tz[0-9]+\.d, z[0-9]+\.d, #257\n} } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.d, z[0-9]+\.d, #65280\n} 1 } } */
/* { dg-final { scan-assembler-not {\tadd\tz[0-9]+\.d, z[0-9]+\.d, #65281\n} } } */
/* { dg-final { scan-assembler-not {\tadd\tz[0-9]+\.d, z[0-9]+\.d, #-1\n} } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.d, z[0-9]+\.d, #1\n} 1 } } */

/* { dg-final { scan-assembler-not {\tsub\tz[0-9]+\.b, z[0-9]+\.b, #1\n} } } */
/* { dg-final { scan-assembler-not {\tsub\tz[0-9]+\.b, z[0-9]+\.b, #5\n} } } */
/* { dg-final { scan-assembler-not {\tsub\tz[0-9]+\.b, z[0-9]+\.b, #255\n} } } */
/* { dg-final { scan-assembler-not {\tsub\tz[0-9]+\.b, z[0-9]+\.b, #256\n} } } */
/* { dg-final { scan-assembler-not {\tsub\tz[0-9]+\.b, z[0-9]+\.b, #257\n} } } */
/* { dg-final { scan-assembler-not {\tsub\tz[0-9]+\.b, z[0-9]+\.b, #65280\n} } } */
/* { dg-final { scan-assembler-not {\tsub\tz[0-9]+\.b, z[0-9]+\.b, #65281\n} } } */
/* { dg-final { scan-assembler-not {\tsub\tz[0-9]+\.b, z[0-9]+\.b, #-1\n} } } */

/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.h, z[0-9]+\.h, #5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.h, z[0-9]+\.h, #255\n} 2 } } */
/* { dg-final { scan-assembler-not {\tsub\tz[0-9]+\.h, z[0-9]+\.h, #256\n} } } */
/* { dg-final { scan-assembler-not {\tsub\tz[0-9]+\.h, z[0-9]+\.h, #257\n} } } */
/* { dg-final { scan-assembler-not {\tsub\tz[0-9]+\.h, z[0-9]+\.h, #65280\n} } } */
/* { dg-final { scan-assembler-not {\tsub\tz[0-9]+\.h, z[0-9]+\.h, #65281\n} } } */
/* { dg-final { scan-assembler-not {\tsub\tz[0-9]+\.h, z[0-9]+\.h, #-1\n} } } */

/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.s, z[0-9]+\.s, #5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.s, z[0-9]+\.s, #255\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.s, z[0-9]+\.s, #256\n} 1 } } */
/* { dg-final { scan-assembler-not {\tsub\tz[0-9]+\.s, z[0-9]+\.s, #257\n} } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.s, z[0-9]+\.s, #65280\n} 1 } } */
/* { dg-final { scan-assembler-not {\tsub\tz[0-9]+\.s, z[0-9]+\.s, #65281\n} } } */
/* { dg-final { scan-assembler-not {\tsub\tz[0-9]+\.s, z[0-9]+\.s, #-1\n} } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.s, z[0-9]+\.s, #1\n} 1 } } */

/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.d, z[0-9]+\.d, #5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.d, z[0-9]+\.d, #255\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.d, z[0-9]+\.d, #256\n} 1 } } */
/* { dg-final { scan-assembler-not {\tsub\tz[0-9]+\.d, z[0-9]+\.d, #257\n} } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.d, z[0-9]+\.d, #65280\n} 1 } } */
/* { dg-final { scan-assembler-not {\tsub\tz[0-9]+\.d, z[0-9]+\.d, #65281\n} } } */
/* { dg-final { scan-assembler-not {\tsub\tz[0-9]+\.d, z[0-9]+\.d, #-1\n} } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.d, z[0-9]+\.d, #1\n} 1 } } */
