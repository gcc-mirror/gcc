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
  DO_IMMEDIATE_OPS (86, TYPE, OP, NAME ## 86);		\
  DO_IMMEDIATE_OPS (109, TYPE, OP, NAME ## 109);	\
  DO_IMMEDIATE_OPS (141, TYPE, OP, NAME ## 141);	\
  DO_IMMEDIATE_OPS (-1, TYPE, OP, NAME ## minus1);	\
  DO_IMMEDIATE_OPS (-110, TYPE, OP, NAME ## minus110);	\
  DO_IMMEDIATE_OPS (-141, TYPE, OP, NAME ## minus141);

DO_ARITH_OPS (int8_t, *, mul)
DO_ARITH_OPS (int16_t, *, mul)
DO_ARITH_OPS (int32_t, *, mul)
DO_ARITH_OPS (int64_t, *, mul)

/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.b, p[0-7]/m, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.b, z[0-9]+\.b, #86\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.b, z[0-9]+\.b, #109\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.b, z[0-9]+\.b, #115\n} 1 } } */
/* { dg-final { scan-assembler-not {\tmul\tz[0-9]+\.b, z[0-9]+\.b, #141\n} } } */
/* { dg-final { scan-assembler-not {\tmul\tz[0-9]+\.b, z[0-9]+\.b, #-1\n} } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.b, z[0-9]+\.b, #-110\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.b, z[0-9]+\.b, #-115\n} 1 } } */
/* { dg-final { scan-assembler-not {\tmul\tz[0-9]+\.b, z[0-9]+\.b, #-141\n} } } */

/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 3 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.h, z[0-9]+\.h, #86\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.h, z[0-9]+\.h, #109\n} 1 } } */
/* { dg-final { scan-assembler-not {\tmul\tz[0-9]+\.h, z[0-9]+\.h, #141\n} } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.h, z[0-9]+\.h, #-110\n} 1 } } */
/* { dg-final { scan-assembler-not {\tmul\tz[0-9]+\.h, z[0-9]+\.h, #-141\n} } } */

/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 3 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.s, z[0-9]+\.s, #86\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.s, z[0-9]+\.s, #109\n} 1 } } */
/* { dg-final { scan-assembler-not {\tmul\tz[0-9]+\.s, z[0-9]+\.s, #141\n} } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.s, z[0-9]+\.s, #-110\n} 1 } } */
/* { dg-final { scan-assembler-not {\tmul\tz[0-9]+\.s, z[0-9]+\.s, #-141\n} } } */

/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 3 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.d, z[0-9]+\.d, #86\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.d, z[0-9]+\.d, #109\n} 1 } } */
/* { dg-final { scan-assembler-not {\tmul\tz[0-9]+\.d, z[0-9]+\.d, #141\n} } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.d, z[0-9]+\.d, #-110\n} 1 } } */
/* { dg-final { scan-assembler-not {\tmul\tz[0-9]+\.d, z[0-9]+\.d, #-141\n} } } */
