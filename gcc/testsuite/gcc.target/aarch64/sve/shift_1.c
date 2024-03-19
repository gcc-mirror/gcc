/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize --save-temps" } */

#include <stdint.h>

#define DO_REG_OPS(TYPE)					\
void ashiftr_##TYPE (TYPE *dst, TYPE src, int count)		\
{								\
  for (int i = 0; i < count; ++i)				\
    dst[i] = dst[i] >> src;					\
}								\
void lshiftr_##TYPE (u##TYPE *dst, u##TYPE src, int count)	\
{								\
  for (int i = 0; i < count; ++i)				\
    dst[i] = dst[i] >> src;					\
}								\
void lshiftl_##TYPE (u##TYPE *dst, u##TYPE src, int count)	\
{								\
  for (int i = 0; i < count; ++i)				\
    dst[i] = dst[i] << src;					\
}								\
void vashiftr_##TYPE (TYPE *dst, TYPE *src, int count)		\
{								\
  for (int i = 0; i < count; ++i)				\
    dst[i] = dst[i] >> src[i];					\
}								\
void vlshiftr_##TYPE (u##TYPE *dst, u##TYPE *src, int count)	\
{								\
  for (int i = 0; i < count; ++i)				\
    dst[i] = dst[i] >> src[i];					\
}								\
void vlshiftl_##TYPE (u##TYPE *dst, u##TYPE *src, int count)	\
{								\
  for (int i = 0; i < count; ++i)				\
    dst[i] = dst[i] << src[i];					\
}

#define DO_IMMEDIATE_OPS(VALUE, TYPE, NAME)			\
void vashiftr_imm_##NAME##_##TYPE (TYPE *dst, int count)	\
{								\
  for (int i = 0; i < count; ++i)				\
    dst[i] = dst[i] >> VALUE;					\
}								\
void vlshiftr_imm_##NAME##_##TYPE (u##TYPE *dst, int count)	\
{								\
  for (int i = 0; i < count; ++i)				\
    dst[i] = dst[i] >> VALUE;					\
}								\
void vlshiftl_imm_##NAME##_##TYPE (u##TYPE *dst, int count)	\
{								\
  for (int i = 0; i < count; ++i)				\
    dst[i] = dst[i] << VALUE;					\
}

DO_REG_OPS (int32_t);
DO_REG_OPS (int64_t);

DO_IMMEDIATE_OPS (0, int8_t, 0);
DO_IMMEDIATE_OPS (5, int8_t, 5);
DO_IMMEDIATE_OPS (7, int8_t, 7);

DO_IMMEDIATE_OPS (0, int16_t, 0);
DO_IMMEDIATE_OPS (5, int16_t, 5);
DO_IMMEDIATE_OPS (15, int16_t, 15);

DO_IMMEDIATE_OPS (0, int32_t, 0);
DO_IMMEDIATE_OPS (5, int32_t, 5);
DO_IMMEDIATE_OPS (31, int32_t, 31);

DO_IMMEDIATE_OPS (0, int64_t, 0);
DO_IMMEDIATE_OPS (5, int64_t, 5);
DO_IMMEDIATE_OPS (63, int64_t, 63);

/* { dg-final { scan-assembler-times {\tasrr?\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tlsrr?\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tlslr?\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */

/* { dg-final { scan-assembler-times {\tasrr?\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 2 } } */
/* { dg-final { scan-assembler-times {\tlsrr?\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 2 } } */
/* { dg-final { scan-assembler-times {\tlslr?\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 2 } } */

/* { dg-final { scan-assembler-times {\tasr\tz[0-9]+\.b, z[0-9]+\.b, #5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlsr\tz[0-9]+\.b, z[0-9]+\.b, #5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.b, z[0-9]+\.b, #5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tasr\tz[0-9]+\.b, z[0-9]+\.b, #7\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlsr\tz[0-9]+\.b, z[0-9]+\.b, #7\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.b, z[0-9]+\.b, #7\n} 1 } } */

/* { dg-final { scan-assembler-times {\tasr\tz[0-9]+\.h, z[0-9]+\.h, #5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlsr\tz[0-9]+\.h, z[0-9]+\.h, #5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.h, z[0-9]+\.h, #5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tasr\tz[0-9]+\.h, z[0-9]+\.h, #15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlsr\tz[0-9]+\.h, z[0-9]+\.h, #15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.h, z[0-9]+\.h, #15\n} 1 } } */

/* { dg-final { scan-assembler-times {\tasr\tz[0-9]+\.s, z[0-9]+\.s, #5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlsr\tz[0-9]+\.s, z[0-9]+\.s, #5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.s, z[0-9]+\.s, #5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tasr\tz[0-9]+\.s, z[0-9]+\.s, #31\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlsr\tz[0-9]+\.s, z[0-9]+\.s, #31\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.s, z[0-9]+\.s, #31\n} 1 } } */

/* { dg-final { scan-assembler-times {\tasr\tz[0-9]+\.d, z[0-9]+\.d, #5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlsr\tz[0-9]+\.d, z[0-9]+\.d, #5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.d, z[0-9]+\.d, #5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tasr\tz[0-9]+\.d, z[0-9]+\.d, #63\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlsr\tz[0-9]+\.d, z[0-9]+\.d, #63\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.d, z[0-9]+\.d, #63\n} 1 } } */
