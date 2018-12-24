/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O3 --save-temps" } */

#include <stdint.h>

#define DO_OPS(TYPE)					\
void vneg_##TYPE (TYPE *dst, TYPE *src, int count)	\
{							\
  for (int i = 0; i < count; ++i)			\
    dst[i] = src[i] < 0 ? -src[i] : src[i];		\
}

DO_OPS (int8_t)
DO_OPS (int16_t)
DO_OPS (int32_t)
DO_OPS (int64_t)

/* { dg-final { scan-assembler-times {\tabs\tz[0-9]+\.b, p[0-7]/m, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tabs\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tabs\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tabs\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d\n} 1 } } */
